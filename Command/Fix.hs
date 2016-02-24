{- git-annex command
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Fix where

import Command
import Config
import qualified Annex
import Annex.Version
import Annex.ReplaceFile
import Annex.Content
import Annex.Perms
import qualified Annex.Queue
import qualified Database.Keys
#ifdef WITH_CLIBS
#ifndef __ANDROID__
import Utility.Touch
#endif
#endif

cmd :: Command
cmd = noCommit $ withGlobalOptions annexedMatchingOptions $
	command "fix" SectionMaintenance
		"fix up links to annexed content"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = unlessM crippledFileSystem $ do 
	fixwhat <- ifM versionSupportsUnlockedPointers
		( return FixAll
		, return FixSymlinks
		)
	flip withFilesInGit ps $ whenAnnexed $ start fixwhat

data FixWhat = FixSymlinks | FixAll

start :: FixWhat -> FilePath -> Key -> CommandStart
start fixwhat file key = do
	currlink <- liftIO $ catchMaybeIO $ readSymbolicLink file
	wantlink <- calcRepo $ gitAnnexLink file key
	case currlink of
		Just l
			| l /= wantlink -> fixby $ fixSymlink file wantlink
			| otherwise -> stop
		Nothing -> case fixwhat of
			FixAll -> fixthin
			FixSymlinks -> stop
  where
	fixby a = do
		showStart "fix" file
		next a
	fixthin = do
		obj <- calcRepo $ gitAnnexLocation key
		stopUnless (isUnmodified key file <&&> isUnmodified key obj) $ do
			thin <- annexThin <$> Annex.getGitConfig
			fs <- liftIO $ catchMaybeIO $ getFileStatus file
			os <- liftIO $ catchMaybeIO $ getFileStatus obj
			case (linkCount <$> fs, linkCount <$> os, thin) of
				(Just 1, Just 1, True) ->
					fixby $ makeHardLink file key
				(Just n, Just n', False) | n > 1 && n == n' ->
					fixby $ breakHardLink file key obj
				_ -> stop

breakHardLink :: FilePath -> Key -> FilePath -> CommandPerform
breakHardLink file key obj = do
	replaceFile file $ \tmp -> do
		unlessM (checkedCopyFile key obj tmp) $
			error "unable to break hard link"
		thawContent tmp
		modifyContent obj $ freezeContent obj
	Database.Keys.storeInodeCaches key [file]
	next $ return True

makeHardLink :: FilePath -> Key -> CommandPerform
makeHardLink file key = do
	replaceFile file $ \tmp -> do
		r <- linkFromAnnex key tmp
		case r of
			LinkAnnexFailed -> error "unable to make hard link"
			_ -> noop
	next $ return True

fixSymlink :: FilePath -> FilePath -> CommandPerform
fixSymlink file link = do
	liftIO $ do
#ifdef WITH_CLIBS
#ifndef __ANDROID__
		-- preserve mtime of symlink
		mtime <- catchMaybeIO $ TimeSpec . modificationTime
			<$> getSymbolicLinkStatus file
#endif
#endif
		createDirectoryIfMissing True (parentDir file)
		removeFile file
		createSymbolicLink link file
#ifdef WITH_CLIBS
#ifndef __ANDROID__
		maybe noop (\t -> touch file t False) mtime
#endif
#endif
	next $ cleanupSymlink file

cleanupSymlink :: FilePath -> CommandCleanup
cleanupSymlink file = do
	Annex.Queue.addCommand "add" [Param "--force", Param "--"] [file]
	return True
