{- git-annex command
 -
 - Copyright 2010-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Unannex where

import Command
import qualified Annex
import Annex.Content
import Annex.Version
import qualified Git.Command
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.DiffTree as DiffTree
import Utility.CopyFile
import Command.PreCommit (lockPreCommitHook)
import qualified Database.Keys
import Git.FilePath

cmd :: Command
cmd = withGlobalOptions annexedMatchingOptions $
	command "unannex" SectionUtility
		"undo accidential add command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek = wrapUnannex . (withFilesInGit $ whenAnnexed start)

wrapUnannex :: Annex a -> Annex a
wrapUnannex a = ifM versionSupportsUnlockedPointers
	( a
	{- Run with the pre-commit hook disabled, to avoid confusing
	 - behavior if an unannexed file is added back to git as
	 - a normal, non-annexed file and then committed.
	 - Otherwise, the pre-commit hook would think that the file
	 - has been unlocked and needs to be re-annexed.
	 -
	 - At the end, make a commit removing the unannexed files.
	 -}
	, ifM cleanindex
		( lockPreCommitHook $ commit `after` a
		, error "Cannot proceed with uncommitted changes staged in the index. Recommend you: git commit"
		)
	)
  where
	commit = inRepo $ Git.Branch.commitCommand Git.Branch.ManualCommit
		[ Param "-q"
		, Param "--allow-empty"
		, Param "--no-verify"
		, Param "-m", Param "content removed from git annex"
		]
	cleanindex = ifM (inRepo Git.Ref.headExists)
		( do
			(diff, cleanupi) <- inRepo $ DiffTree.diffIndex Git.Ref.headRef
			if null diff
				then void (liftIO cleanupi) >> return True
				else void (liftIO cleanupi) >> return False
		, return False
		)

start :: FilePath -> Key -> CommandStart
start file key = stopUnless (inAnnex key) $ do
	showStart "unannex" file
	next $ perform file key

perform :: FilePath -> Key -> CommandPerform
perform file key = do
	liftIO $ removeFile file
	inRepo $ Git.Command.run
		[ Param "rm"
		, Param "--cached"
		, Param "--force"
		, Param "--quiet"
		, Param "--"
		, File file
		]
	next $ cleanup file key

cleanup :: FilePath -> Key -> CommandCleanup
cleanup file key = do
	Database.Keys.removeAssociatedFile key =<< inRepo (toTopFilePath file)
	src <- calcRepo $ gitAnnexLocation key
	ifM (Annex.getState Annex.fast)
		( do
			-- Only make a hard link if the annexed file does not
			-- already have other hard links pointing at it.
			-- This avoids unannexing (and uninit) ending up
			-- hard linking files together, which would be
			-- surprising.
			s <- liftIO $ getFileStatus src
			if linkCount s > 1
				then copyfrom src
				else hardlinkfrom src
		, copyfrom src
		)
  where
	copyfrom src = 
		thawContent file `after` liftIO (copyFileExternal CopyAllMetaData src file)
	hardlinkfrom src =
#ifndef mingw32_HOST_OS
		-- creating a hard link could fall; fall back to copying
		ifM (liftIO $ catchBoolIO $ createLink src file >> return True)
			( return True
			, copyfrom src
			)
#else
		copyfrom src
#endif
