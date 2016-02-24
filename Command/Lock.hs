{- git-annex command
 -
 - Copyright 2010,2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Lock where

import Command
import qualified Annex.Queue
import qualified Annex
import Annex.Version
import Annex.Content
import Annex.Link
import Annex.InodeSentinal
import Annex.Perms
import Annex.ReplaceFile
import Utility.InodeCache
import qualified Database.Keys
import Annex.Ingest
import Logs.Location
import Git.FilePath
	
cmd :: Command
cmd = withGlobalOptions annexedMatchingOptions $
	command "lock" SectionCommon
		"undo unlock command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = ifM versionSupportsUnlockedPointers
	( withFilesInGit (whenAnnexed startNew) ps
	, do
		withFilesOldUnlocked startOld ps
		withFilesOldUnlockedToBeCommitted startOld ps
	)

startNew :: FilePath -> Key -> CommandStart
startNew file key = ifM (isJust <$> isAnnexLink file)
	( stop
	, do
		showStart "lock" file
		go =<< liftIO (isPointerFile file)
	)
  where
	go (Just key')
		| key' == key = error "content not present; cannot lock"
		| otherwise = errorModified
	go Nothing = 
		ifM (isUnmodified key file) 
			( cont False
			, ifM (Annex.getState Annex.force)
				( cont True
				, errorModified
				)
			)
	cont = next . performNew file key

performNew :: FilePath -> Key -> Bool -> CommandPerform
performNew file key filemodified = do
	lockdown =<< calcRepo (gitAnnexLocation key)
	addLink file key
		=<< withTSDelta (liftIO . genInodeCache file)
	next $ cleanupNew file key
  where
	lockdown obj = do
		ifM (catchBoolIO $ sameInodeCache obj =<< Database.Keys.getInodeCaches key)
			( breakhardlink obj
			, repopulate obj
			)
		whenM (liftIO $ doesFileExist obj) $
			freezeContent obj

	-- It's ok if the file is hard linked to obj, but if some other
	-- associated file is, we need to break that link to lock down obj.
	breakhardlink obj = whenM (catchBoolIO $ (> 1) . linkCount <$> liftIO (getFileStatus obj)) $ do
		mfc <- withTSDelta (liftIO . genInodeCache file)
		unlessM (sameInodeCache obj (maybeToList mfc)) $ do
			modifyContent obj $ replaceFile obj $ \tmp -> do
				unlessM (checkedCopyFile key obj tmp) $
					error "unable to lock file"
			Database.Keys.storeInodeCaches key [obj]

	-- Try to repopulate obj from an unmodified associated file.
	repopulate obj
		| filemodified = modifyContent obj $ do
			g <- Annex.gitRepo
			fs <- map (`fromTopFilePath` g)
				<$> Database.Keys.getAssociatedFiles key
			mfile <- firstM (isUnmodified key) fs
			liftIO $ nukeFile obj
			case mfile of
				Just unmodified ->
					unlessM (checkedCopyFile key unmodified obj)
						lostcontent
				Nothing -> lostcontent
		| otherwise = modifyContent obj $ 
			liftIO $ renameFile file obj
	lostcontent = logStatus key InfoMissing

cleanupNew :: FilePath -> Key -> CommandCleanup
cleanupNew file key = do
	Database.Keys.removeAssociatedFile key =<< inRepo (toTopFilePath file)
	return True

startOld :: FilePath -> CommandStart
startOld file = do
	showStart "lock" file
	unlessM (Annex.getState Annex.force)
		errorModified
	next $ performOld file

performOld :: FilePath -> CommandPerform
performOld file = do
	Annex.Queue.addCommand "checkout" [Param "--"] [file]
	next $ return True

errorModified :: a
errorModified =  error "Locking this file would discard any changes you have made to it. Use 'git annex add' to stage your changes. (Or, use --force to override)"
