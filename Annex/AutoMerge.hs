{- git-annex automatic merge conflict resolution
 -
 - Copyright 2012-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.AutoMerge
	( autoMergeFrom
	, resolveMerge
	, commitResolvedMerge
	) where

import Annex.Common
import qualified Annex.Queue
import Annex.CatFile
import Annex.Link
import Annex.Content
import qualified Git.LsFiles as LsFiles
import qualified Git.UpdateIndex as UpdateIndex
import qualified Git.Merge
import qualified Git.Ref
import qualified Git
import qualified Git.Branch
import Git.Types (BlobType(..))
import Git.FilePath
import Annex.ReplaceFile
import Annex.VariantFile
import qualified Database.Keys
import Annex.InodeSentinal
import Utility.InodeCache

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L

{- Merges from a branch into the current branch (which may not exist yet),
 - with automatic merge conflict resolution.
 -
 - Callers should use Git.Branch.changed first, to make sure that
 - there are changes from the current branch to the branch being merged in.
 -}
autoMergeFrom :: Git.Ref -> Maybe Git.Ref -> Git.Branch.CommitMode -> Annex Bool
autoMergeFrom branch currbranch commitmode = do
	showOutput
	case currbranch of
		Nothing -> go Nothing
		Just b -> go =<< inRepo (Git.Ref.sha b)
  where
	go old = inRepo (Git.Merge.mergeNonInteractive branch commitmode)
		<||> (resolveMerge old branch <&&> commitResolvedMerge commitmode)

{- Resolves a conflicted merge. It's important that any conflicts be
 - resolved in a way that itself avoids later merge conflicts, since
 - multiple repositories may be doing this concurrently.
 -
 - Only merge conflicts where at least one side is an annexed file
 - is resolved.
 -
 - This uses the Keys pointed to by the files to construct new
 - filenames. So when both sides modified annexed file foo, 
 - it will be deleted, and replaced with files foo.variant-A and
 - foo.variant-B.
 -
 - On the other hand, when one side deleted foo, and the other modified it,
 - it will be deleted, and the modified version stored as file
 - foo.variant-A (or B).
 -
 - It's also possible that one side has foo as an annexed file, and
 - the other as a directory or non-annexed file. The annexed file
 - is renamed to resolve the merge, and the other object is preserved as-is.
 -
 - In indirect mode, the merge is resolved in the work tree and files
 - staged, to clean up from a conflicted merge that was run in the work
 - tree.
 -
 - In direct mode, the work tree is not touched here; files are staged to
 - the index, and written to the gitAnnexMergeDir, for later handling by
 - the direct mode merge code.
 -
 - Unlocked files remain unlocked after merging, and locked files
 - remain locked. When the merge conflict is between a locked and unlocked
 - file, that otherwise point to the same content, the unlocked mode wins.
 - This is done because only unlocked files work in filesystems that don't
 - support symlinks.
 -
 - Returns false when there are no merge conflicts to resolve.
 - A git merge can fail for other reasons, and this allows detecting
 - such failures.
 -}
resolveMerge :: Maybe Git.Ref -> Git.Ref -> Annex Bool
resolveMerge us them = do
	top <- fromRepo Git.repoPath
	(fs, cleanup) <- inRepo (LsFiles.unmerged [top])
	srcmap <- inodeMap $ pure (map LsFiles.unmergedFile fs, return True)
	(mergedks, mergedfs) <- unzip <$> mapM (resolveMerge' srcmap us them) fs
	let mergedks' = concat mergedks
	let mergedfs' = catMaybes mergedfs
	let merged = not (null mergedfs')
	void $ liftIO cleanup

	(deleted, cleanup2) <- inRepo (LsFiles.deleted [top])
	unless (null deleted) $
		Annex.Queue.addCommand "rm"
			[Param "--quiet", Param "-f", Param "--"]
			deleted
	void $ liftIO cleanup2

	when merged $ do
		Annex.Queue.flush
		unstagedmap <- inodeMap $ inRepo $ LsFiles.notInRepo False [top]
		cleanConflictCruft mergedks' mergedfs' unstagedmap
		showLongNote "Merge conflict was automatically resolved; you may want to examine the result."
	return merged

resolveMerge' :: InodeMap -> Maybe Git.Ref -> Git.Ref -> LsFiles.Unmerged -> Annex ([Key], Maybe FilePath)
resolveMerge' _ Nothing _ _ = return ([], Nothing)
resolveMerge' unstagedmap (Just us) them u = do
	kus <- getkey LsFiles.valUs
	kthem <- getkey LsFiles.valThem
	case (kus, kthem) of
		-- Both sides of conflict are annexed files
		(Just keyUs, Just keyThem)
			| keyUs /= keyThem -> resolveby [keyUs, keyThem] $ do
				makeannexlink keyUs LsFiles.valUs
				makeannexlink keyThem LsFiles.valThem
				-- cleanConflictCruft can't handle unlocked
				-- files, so delete here.
				unless (islocked LsFiles.valUs) $
					liftIO $ nukeFile file
			| otherwise -> do
				-- Only resolve using symlink when both
				-- were locked, otherwise use unlocked
				-- pointer.
				-- In either case, keep original filename.
				if islocked LsFiles.valUs && islocked LsFiles.valThem
					then makesymlink keyUs file
					else makepointer keyUs file
				return ([keyUs, keyThem], Just file)
		-- Our side is annexed file, other side is not.
		(Just keyUs, Nothing) -> resolveby [keyUs] $ do
			graftin them file LsFiles.valThem LsFiles.valThem LsFiles.valUs
			makeannexlink keyUs LsFiles.valUs
		-- Our side is not annexed file, other side is.
		(Nothing, Just keyThem) -> resolveby [keyThem] $ do
			graftin us file LsFiles.valUs LsFiles.valUs LsFiles.valThem
			makeannexlink keyThem LsFiles.valThem
		-- Neither side is annexed file; cannot resolve.
		(Nothing, Nothing) -> return ([], Nothing)
  where
	file = LsFiles.unmergedFile u

	getkey select = 
		case select (LsFiles.unmergedSha u) of
			Just sha -> catKey sha
			Nothing -> return Nothing
	
	islocked select = select (LsFiles.unmergedBlobType u) == Just SymlinkBlob

	makeannexlink key select
		| islocked select = makesymlink key dest
		| otherwise = makepointer key dest
	  where
		dest = variantFile file key

	makesymlink key dest = do
		l <- calcRepo $ gitAnnexLink dest key
		replacewithsymlink dest l
		stageSymlink dest =<< hashSymlink l

	replacewithsymlink dest link = withworktree dest $ \f ->
		replaceFile f $ makeGitLink link

	makepointer key dest = do
		unlessM (reuseOldFile unstagedmap key file dest) $ do
			r <- linkFromAnnex key dest
			case r of
				LinkAnnexFailed -> liftIO $
					writeFile dest (formatPointer key)
				_ -> noop
		stagePointerFile dest =<< hashPointerFile key
		Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath dest)

	withworktree f a = a f

	{- Stage a graft of a directory or file from a branch
	 - and update the work tree. -}
	graftin b item selectwant selectwant' selectunwant = do
		Annex.Queue.addUpdateIndex
			=<< fromRepo (UpdateIndex.lsSubTree b item)

		-- Update the work tree to reflect the graft.
		case (selectwant (LsFiles.unmergedBlobType u), selectunwant (LsFiles.unmergedBlobType u)) of
			-- Symlinks are never left in work tree when
			-- there's a conflict with anything else.
			-- So, when grafting in a symlink, we must create it:
			(Just SymlinkBlob, _) -> do
				case selectwant' (LsFiles.unmergedSha u) of
					Nothing -> noop
					Just sha -> do
						link <- catSymLinkTarget sha
						replacewithsymlink item link
			-- And when grafting in anything else vs a symlink,
			-- the work tree already contains what we want.
			(_, Just SymlinkBlob) -> noop
			_ -> ifM (withworktree item (liftIO . doesDirectoryExist))
				-- a conflict between a file and a directory
				-- leaves the directory, so since a directory
				-- is there, it must be what was wanted
				( noop
				-- probably a file with conflict markers is
				-- in the work tree; replace with grafted
				-- file content
				, case selectwant' (LsFiles.unmergedSha u) of
					Nothing -> noop
					Just sha -> withworktree item $ \f -> 
						replaceFile f $ \tmp -> do
							c <- catObject sha
							liftIO $ L.writeFile tmp c
				)
	
	resolveby ks a = do
		{- Remove conflicted file from index so merge can be resolved. -}
		Annex.Queue.addCommand "rm"
			[Param "--quiet", Param "-f", Param "--cached", Param "--"] [file]
		void a
		return (ks, Just file)

{- git-merge moves conflicting files away to files
 - named something like f~HEAD or f~branch or just f, but the
 - exact name chosen can vary. Once the conflict is resolved,
 - this cruft can be deleted. To avoid deleting legitimate
 - files that look like this, only delete files that are
 - A) not staged in git and
 - B) have a name related to the merged files and
 - C) are pointers to or have the content of keys that were involved
 - in the merge.
 -}
cleanConflictCruft :: [Key] -> [FilePath] -> InodeMap -> Annex ()
cleanConflictCruft resolvedks resolvedfs unstagedmap = do
	is <- S.fromList . map (inodeCacheToKey Strongly) . concat 
		<$> mapM Database.Keys.getInodeCaches resolvedks
	forM_ (M.toList unstagedmap) $ \(i, f) ->
		whenM (matchesresolved is i f) $
			liftIO $ nukeFile f
  where
	fs = S.fromList resolvedfs
	ks = S.fromList resolvedks
	inks = maybe False (flip S.member ks)
	matchesresolved is i f
		| S.member f fs || S.member (conflictCruftBase f) fs = anyM id
			[ pure (S.member i is)
			, inks <$> isAnnexLink f
			, inks <$> liftIO (isPointerFile f)
			]
		| otherwise = return False

conflictCruftBase :: FilePath -> FilePath
conflictCruftBase f = reverse $ drop 1 $ dropWhile (/= '~') $ reverse f

{- When possible, reuse an existing file from the srcmap as the
 - content of a worktree file in the resolved merge. It must have the
 - same name as the origfile, or a name that git would use for conflict
 - cruft. And, its inode cache must be a known one for the key. -}
reuseOldFile :: InodeMap -> Key -> FilePath -> FilePath -> Annex Bool
reuseOldFile srcmap key origfile destfile = do
	is <- map (inodeCacheToKey Strongly)
		<$> Database.Keys.getInodeCaches key
	liftIO $ go $ mapMaybe (\i -> M.lookup i srcmap) is
  where
	go [] = return False
	go (f:fs)
		| f == origfile || conflictCruftBase f == origfile = 
			ifM (doesFileExist f)
				( do
					renameFile f destfile
					return True
				, go fs
				)
		| otherwise = go fs
	
commitResolvedMerge :: Git.Branch.CommitMode -> Annex Bool
commitResolvedMerge commitmode = inRepo $ Git.Branch.commitCommand commitmode
	[ Param "--no-verify"
	, Param "-m"
	, Param "git-annex automatic merge conflict fix"
	]

type InodeMap = M.Map InodeCacheKey FilePath

inodeMap :: Annex ([FilePath], IO Bool) -> Annex InodeMap
inodeMap getfiles = do
	(fs, cleanup) <- getfiles
	fsis <- forM fs $ \f -> do
		mi <- withTSDelta (liftIO . genInodeCache f)
		return $ case mi of
			Nothing -> Nothing
			Just i -> Just (inodeCacheToKey Strongly i, f)
	void $ liftIO cleanup
	return $ M.fromList $ catMaybes fsis
