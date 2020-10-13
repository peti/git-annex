{- git ls-files interface
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Git.LsFiles (
	Options(..),
	inRepo,
	inRepoDetails,
	inRepoOrBranch,
	notInRepo,
	notInRepoIncludingEmptyDirectories,
	allFiles,
	deleted,
	modified,
	staged,
	stagedNotDeleted,
	usualStageNum,
	mergeConflictHeadStageNum,
	stagedDetails,
	typeChanged,
	typeChangedStaged,
	Conflicting(..),
	Unmerged(..),
	unmerged,
	StagedDetails,
	inodeCaches,
) where

import Common
import Git
import Git.Command
import Git.Types
import Git.Sha
import Utility.InodeCache
import Utility.TimeStamp
import Utility.Attoparsec

import System.Posix.Types
import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8

{- It's only safe to use git ls-files on the current repo, not on a remote.
 -
 - Git has some strange behavior when git ls-files is used with repos
 - that are not the one that the cwd is in:
 - git --git-dir=../foo/.git --worktree=../foo ../foo fails saying 
 - "../foo is outside repository".
 - That does not happen when an absolute path is provided.
 -
 - Also, the files output by ls-files are relative to the cwd. 
 - Unless it's run on remote. Then it's relative to the top of the remote
 - repo.
 -
 - So, best to avoid that class of problems.
 -}
safeForLsFiles :: Repo -> Bool
safeForLsFiles r = isNothing (remoteName r)

guardSafeForLsFiles :: Repo -> IO a -> IO a
guardSafeForLsFiles r a
	| safeForLsFiles r = a
	| otherwise = error $ "git ls-files is unsafe to run on repository " ++ repoDescribe r

data Options = ErrorUnmatch

opParam :: Options -> CommandParam
opParam ErrorUnmatch = Param "--error-unmatch"

{- Lists files that are checked into git's index at the specified paths.
 - With no paths, all files are listed.
 -}
inRepo :: [Options] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
inRepo = inRepo' [Param "--cached"] 

inRepo' :: [CommandParam] -> [Options] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
inRepo' ps os l repo = guardSafeForLsFiles repo $ pipeNullSplit' params repo
  where
	params = 
		Param "ls-files" :
		Param "-z" :
		map opParam os ++ ps ++
		(Param "--" : map (File . fromRawFilePath) l)

{- Lists the same files inRepo does, but with sha and mode. -}
inRepoDetails :: [Options] -> [RawFilePath] -> Repo -> IO ([(RawFilePath, Sha, FileMode)], IO Bool)
inRepoDetails = stagedDetails' parser . map opParam
  where
	parser s = case parseStagedDetails s of
		Just (file, sha, mode, stagenum)
			| stagenum == usualStageNum || stagenum == mergeConflictHeadStageNum ->
				Just (file, sha, mode)
		_ -> Nothing

{- Files that are checked into the index or have been committed to a
 - branch. -}
inRepoOrBranch :: Branch -> [Options] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
inRepoOrBranch b = inRepo'
	[ Param "--cached"
	, Param ("--with-tree=" ++ fromRef b)
	]

{- Scans for files at the specified locations that are not checked into git. -}
notInRepo :: [Options] -> Bool -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
notInRepo = notInRepo' []

notInRepo' :: [CommandParam] -> [Options] -> Bool -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
notInRepo' ps os include_ignored = 
	inRepo' (Param "--others" : ps ++ exclude) os
  where
	exclude
		| include_ignored = []
		| otherwise = [Param "--exclude-standard"]

{- Scans for files at the specified locations that are not checked into
 - git. Empty directories are included in the result. -}
notInRepoIncludingEmptyDirectories :: [Options] -> Bool -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
notInRepoIncludingEmptyDirectories = notInRepo' [Param "--directory"]

{- Finds all files in the specified locations, whether checked into git or
 - not. -}
allFiles :: [Options] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
allFiles = inRepo' [Param "--cached", Param "--others"]

{- Returns a list of files in the specified locations that have been
 - deleted. -}
deleted :: [Options] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
deleted = inRepo' [Param "--deleted"]

{- Returns a list of files in the specified locations that have been
 - modified. -}
modified :: [Options] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
modified = inRepo' [Param "--modified"]

{- Returns a list of all files that are staged for commit. -}
staged :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
staged = staged' []

{- Returns a list of the files, staged for commit, that are being added,
 - moved, or changed (but not deleted), from the specified locations. -}
stagedNotDeleted :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
stagedNotDeleted = staged' [Param "--diff-filter=ACMRT"]

staged' :: [CommandParam] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
staged' ps l repo = guardSafeForLsFiles repo $
	pipeNullSplit' (prefix ++ ps ++ suffix) repo
  where
	prefix = [Param "diff", Param "--cached", Param "--name-only", Param "-z"]
	suffix = Param "--" : map (File . fromRawFilePath) l

type StagedDetails = (RawFilePath, Sha, FileMode, StageNum)

type StageNum = Int

{- Used when not in a merge conflict. -}
usualStageNum :: Int
usualStageNum = 0

{- WHen in a merge conflict, git uses stage number 2 for the local HEAD
 - side of the merge conflict. -}
mergeConflictHeadStageNum :: Int
mergeConflictHeadStageNum = 2

{- Returns details about all files that are staged in the index.
 -
 - Note that, during a conflict, a file will appear in the list
 - more than once with different stage numbers.
 -}
stagedDetails :: [RawFilePath] -> Repo -> IO ([StagedDetails], IO Bool)
stagedDetails = stagedDetails' parseStagedDetails []

stagedDetails' :: (S.ByteString -> Maybe t) -> [CommandParam] -> [RawFilePath] -> Repo -> IO ([t], IO Bool)
stagedDetails' parser ps l repo = guardSafeForLsFiles repo $ do
	ls <- pipeNullSplit'' params repo
	return (mapMaybe parser ls, return True)
  where
	params = Param "ls-files" : Param "--stage" : Param "-z" : ps ++ 
		Param "--" : map (File . fromRawFilePath) l

parseStagedDetails :: S.ByteString -> Maybe StagedDetails
parseStagedDetails = eitherToMaybe . A.parseOnly parser
  where
	parser = do
		!mode <- octal
		void $ A8.char ' '
		!sha <- maybe (fail "bad sha") return . extractSha . S.copy =<< nextword
		void $ A8.char ' '
		!stagenum <- A8.decimal
		void $ A8.char '\t'
		!file <- S.copy <$> A.takeByteString
		return (file, sha, mode, stagenum)
	
	nextword = A8.takeTill (== ' ')

{- Returns a list of the files in the specified locations that are staged
 - for commit, and whose type has changed. -}
typeChangedStaged :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
typeChangedStaged = typeChanged' [Param "--cached"]

{- Returns a list of the files in the specified locations whose type has
 - changed.  Files only staged for commit will not be included. -}
typeChanged :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
typeChanged = typeChanged' []

typeChanged' :: [CommandParam] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
typeChanged' ps l repo = guardSafeForLsFiles repo $ do
	(fs, cleanup) <- pipeNullSplit (prefix ++ ps ++ suffix) repo
	-- git diff returns filenames relative to the top of the git repo;
	-- convert to filenames relative to the cwd, like git ls-files.
	top <- absPath (fromRawFilePath (repoPath repo))
	currdir <- getCurrentDirectory
	return (map (\f -> toRawFilePath (relPathDirToFileAbs currdir $ top </> decodeBL' f)) fs, cleanup)
  where
	prefix = 
		[ Param "diff"
		, Param "--name-only"
		, Param "--diff-filter=T"
		, Param "-z"
		]
	suffix = Param "--" : (if null l then [File "."] else map (File . fromRawFilePath) l)

{- A item in conflict has two possible values.
 - Either can be Nothing, when that side deleted the file. -}
data Conflicting v = Conflicting
	{ valUs :: Maybe v
	, valThem :: Maybe v
	} deriving (Show)

data Unmerged = Unmerged
	{ unmergedFile :: RawFilePath
	, unmergedTreeItemType :: Conflicting TreeItemType
	, unmergedSha :: Conflicting Sha
	}

{- Returns a list of the files in the specified locations that have
 - unresolved merge conflicts.
 -
 - ls-files outputs multiple lines per conflicting file, each with its own
 - stage number:
 -   1 = old version, can be ignored
 -   2 = us
 -   3 = them
 - If a line is omitted, that side removed the file.
 -}
unmerged :: [RawFilePath] -> Repo -> IO ([Unmerged], IO Bool)
unmerged l repo = guardSafeForLsFiles repo $ do
	(fs, cleanup) <- pipeNullSplit params repo
	return (reduceUnmerged [] $ catMaybes $ map (parseUnmerged . decodeBL') fs, cleanup)
  where
	params = 
		Param "ls-files" :
		Param "--unmerged" :
		Param "-z" :
		Param "--" :
		map (File . fromRawFilePath) l

data InternalUnmerged = InternalUnmerged
	{ isus :: Bool
	, ifile :: RawFilePath
	, itreeitemtype :: Maybe TreeItemType
	, isha :: Maybe Sha
	}

parseUnmerged :: String -> Maybe InternalUnmerged
parseUnmerged s
	| null file = Nothing
	| otherwise = case words metadata of
		(rawtreeitemtype:rawsha:rawstage:_) -> do
			stage <- readish rawstage :: Maybe Int
			if stage /= 2 && stage /= 3
				then Nothing
				else do
					treeitemtype <- readTreeItemType (encodeBS rawtreeitemtype)
					sha <- extractSha (encodeBS' rawsha)
					return $ InternalUnmerged (stage == 2) (toRawFilePath file)
						(Just treeitemtype) (Just sha)
		_ -> Nothing
  where
	(metadata, file) = separate (== '\t') s

reduceUnmerged :: [Unmerged] -> [InternalUnmerged] -> [Unmerged]
reduceUnmerged c [] = c
reduceUnmerged c (i:is) = reduceUnmerged (new:c) rest
  where
	(rest, sibi) = findsib i is
	(treeitemtypeA, treeitemtypeB, shaA, shaB)
		| isus i    = (itreeitemtype i, itreeitemtype sibi, isha i, isha sibi)
		| otherwise = (itreeitemtype sibi, itreeitemtype i, isha sibi, isha i)
	new = Unmerged
		{ unmergedFile = ifile i
		, unmergedTreeItemType = Conflicting treeitemtypeA treeitemtypeB
		, unmergedSha = Conflicting shaA shaB
		}
	findsib templatei [] = ([], removed templatei)
	findsib templatei (l:ls)
		| ifile l == ifile templatei = (ls, l)
		| otherwise = (l:ls, removed templatei)
	removed templatei = templatei
		{ isus = not (isus templatei)
		, itreeitemtype = Nothing
		, isha = Nothing
		}

{- Gets the InodeCache equivilant information stored in the git index.
 -
 - Note that this uses a --debug option whose output could change at some
 - point in the future. If the output is not as expected, will use Nothing.
 -}
inodeCaches :: [RawFilePath] -> Repo -> IO ([(FilePath, Maybe InodeCache)], IO Bool)
inodeCaches locs repo = guardSafeForLsFiles repo $ do
	(ls, cleanup) <- pipeNullSplit params repo
	return (parse Nothing (map decodeBL ls), cleanup)
  where
	params = 
		Param "ls-files" :
		Param "--cached" :
		Param "-z" :
		Param "--debug" :
		Param "--" :
		map (File . fromRawFilePath) locs
	
	parse Nothing (f:ls) = parse (Just f) ls
	parse (Just f) (s:[]) = 
		let i = parsedebug s
		in (f, i) : []
	parse (Just f) (s:ls) =
		let (d, f') = splitdebug s
		    i = parsedebug d
		in (f, i) : parse (Just f') ls
	parse _ _ = []

	-- First 5 lines are --debug output, remainder is the next filename.
	-- This assumes that --debug does not start outputting more lines.
	splitdebug s = case splitc '\n' s of
		(d1:d2:d3:d4:d5:rest) ->
			( intercalate "\n" [d1, d2, d3, d4, d5]
			, intercalate "\n" rest
			)
		_ -> ("", s)
	
	-- This parser allows for some changes to the --debug output,
	-- including reordering, or adding more items.
	parsedebug s = do
		let l = words s
		let iskey v = ":" `isSuffixOf` v
		let m = M.fromList $ zip
			(filter iskey l)
			(filter (not . iskey) l)
		mkInodeCache
			<$> (readish =<< M.lookup "ino:" m)
			<*> (readish =<< M.lookup "size:" m)
			<*> (parsePOSIXTime =<< (replace ":" "." <$> M.lookup "mtime:" m))
