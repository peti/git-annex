{- git-annex command
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Fsck where

import Command
import qualified Annex
import qualified Remote
import qualified Types.Backend
import qualified Backend
import Annex.Content
import Annex.Perms
import Annex.Link
import Logs.Location
import Logs.Trust
import Logs.Activity
import Logs.TimeStamp
import Annex.NumCopies
import Annex.UUID
import Annex.ReplaceFile
import Utility.DataUnits
import Utility.HumanTime
import Utility.CopyFile
import Git.FilePath
import Utility.PID
import qualified Database.Keys
import qualified Database.Fsck as FsckDb
import Types.CleanupActions

import Data.Time.Clock.POSIX
import System.Posix.Types (EpochTime)

cmd :: Command
cmd = withGlobalOptions (jobsOption : annexedMatchingOptions) $
	command "fsck" SectionMaintenance
		"find and fix problems"
		paramPaths (seek <$$> optParser)

data FsckOptions = FsckOptions
	{ fsckFiles :: CmdParams
	, fsckFromOption :: Maybe (DeferredParse Remote)
	, incrementalOpt :: Maybe IncrementalOpt
	, keyOptions :: Maybe KeyOptions
	}

data IncrementalOpt
	= StartIncrementalO
	| MoreIncrementalO
	| ScheduleIncrementalO Duration

optParser :: CmdParamsDesc -> Parser FsckOptions
optParser desc = FsckOptions
	<$> cmdParams desc
	<*> optional (parseRemoteOption $ strOption 
		( long "from" <> short 'f' <> metavar paramRemote 
		<> help "check remote"
		<> completeRemotes
		))
	<*> optional parseincremental
	<*> optional (parseKeyOptions False)
  where
	parseincremental =
		flag' StartIncrementalO
			( long "incremental" <> short 'S'
			<> help "start an incremental fsck"
			)
		<|> flag' MoreIncrementalO
			( long "more" <> short 'm'
			<> help "continue an incremental fsck"
			)
		<|> (ScheduleIncrementalO <$> option (str >>= parseDuration)
			( long "incremental-schedule" <> metavar paramTime
			<> help "schedule incremental fscking"
			))

seek :: FsckOptions -> CommandSeek
seek o = allowConcurrentOutput $ do
	from <- maybe (pure Nothing) (Just <$$> getParsed) (fsckFromOption o)
	u <- maybe getUUID (pure . Remote.uuid) from
	checkDeadRepo u
	i <- prepIncremental u (incrementalOpt o)
	withKeyOptions (keyOptions o) False
		(\k -> startKey i k =<< getNumCopies)
		(withFilesInGit $ whenAnnexed $ start from i)
		(fsckFiles o)
	cleanupIncremental i
	void $ tryIO $ recordActivity Fsck u

checkDeadRepo :: UUID -> Annex ()
checkDeadRepo u =
	whenM ((==) DeadTrusted <$> lookupTrust u) $
		earlyWarning "Warning: Fscking a repository that is currently marked as dead."

start :: Maybe Remote -> Incremental -> FilePath -> Key -> CommandStart
start from inc file key = do
	v <- Backend.getBackend file key
	case v of
		Nothing -> stop
		Just backend -> do
			numcopies <- getFileNumCopies file
			case from of
				Nothing -> go $ perform key file backend numcopies
				Just r -> go $ performRemote key file backend numcopies r
  where
	go = runFsck inc file key

perform :: Key -> FilePath -> Backend -> NumCopies -> Annex Bool
perform key file backend numcopies = do
	keystatus <- getKeyFileStatus key file
	check
		-- order matters
		[ fixLink key file
		, verifyLocationLog key keystatus file
		, verifyAssociatedFiles key keystatus file
		, verifyWorkTree key file
		, checkKeySize key keystatus
		, checkBackend backend key keystatus
		, checkKeyNumCopies key (Just file) numcopies
		]

{- To fsck a remote, the content is retrieved to a tmp file,
 - and checked locally. -}
performRemote :: Key -> FilePath -> Backend -> NumCopies -> Remote -> Annex Bool
performRemote key file backend numcopies remote =
	dispatch =<< Remote.hasKey remote key
  where
	dispatch (Left err) = do
		showNote err
		return False
	dispatch (Right True) = withtmp $ \tmpfile -> do
		r <- getfile tmpfile
		case r of
			Nothing -> go True Nothing
			Just True -> go True (Just tmpfile)
			Just False -> do
				warning "failed to download file from remote"
				void $ go True Nothing
				return False
	dispatch (Right False) = go False Nothing
	go present localcopy = check
		[ verifyLocationLogRemote key file remote present
		, checkKeySizeRemote key remote localcopy
		, checkBackendRemote backend key remote localcopy
		, checkKeyNumCopies key (Just file) numcopies
		]
	withtmp a = do
		pid <- liftIO getPID
		t <- fromRepo gitAnnexTmpObjectDir
		createAnnexDirectory t
		let tmp = t </> "fsck" ++ show pid ++ "." ++ keyFile key
		let cleanup = liftIO $ catchIO (removeFile tmp) (const noop)
		cleanup
		cleanup `after` a tmp
	getfile tmp = ifM (checkDiskSpace (Just (takeDirectory tmp)) key 0 True)
		( ifM (Remote.retrieveKeyFileCheap remote key (Just file) tmp)
			( return (Just True)
			, ifM (Annex.getState Annex.fast)
				( return Nothing
				, Just . fst <$>
					Remote.retrieveKeyFile remote key Nothing tmp dummymeter
				)
			)
		, return (Just False)
		)
	dummymeter _ = noop

startKey :: Incremental -> Key -> NumCopies -> CommandStart
startKey inc key numcopies =
	case Backend.maybeLookupBackendName (keyBackendName key) of
		Nothing -> stop
		Just backend -> runFsck inc (key2file key) key $
			performKey key backend numcopies

performKey :: Key -> Backend -> NumCopies -> Annex Bool
performKey key backend numcopies = do
	keystatus <- getKeyStatus key
	check
		[ verifyLocationLog key keystatus (key2file key)
		, checkKeySize key keystatus
		, checkBackend backend key keystatus
		, checkKeyNumCopies key Nothing numcopies
		]

check :: [Annex Bool] -> Annex Bool
check cs = and <$> sequence cs

{- Checks that symlinks points correctly to the annexed content.
 -}
fixLink :: Key -> FilePath -> Annex Bool
fixLink key file = do
	want <- calcRepo $ gitAnnexLink file key
	have <- getAnnexLinkTarget file
	maybe noop (go want) have
	return True
  where
	go want have
		| want /= fromInternalGitPath have = do
			showNote "fixing link"
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ removeFile file
			addAnnexLink want file
		| otherwise = noop

{- Checks that the location log reflects the current status of the key,
 - in this repository only. -}
verifyLocationLog :: Key -> KeyStatus -> String -> Annex Bool
verifyLocationLog key keystatus desc = do
	obj <- calcRepo $ gitAnnexLocation key
	present <- if isKeyUnlocked keystatus
		then liftIO (doesFileExist obj)
		else inAnnex key
	u <- getUUID
	
	{- Since we're checking that a key's object file is present, throw
	 - in a permission fixup here too. -}
	when (present) $ void $ tryIO $
		if isKeyUnlocked keystatus
			then thawContent obj
			else freezeContent obj
	whenM (liftIO $ doesDirectoryExist $ parentDir obj) $
		freezeContentDir obj

	verifyLocationLog' key desc present u (logChange key u)

verifyLocationLogRemote :: Key -> String -> Remote -> Bool -> Annex Bool
verifyLocationLogRemote key desc remote present =
	verifyLocationLog' key desc present (Remote.uuid remote)
		(Remote.logStatus remote key)

verifyLocationLog' :: Key -> String -> Bool -> UUID -> (LogStatus -> Annex ()) -> Annex Bool
verifyLocationLog' key desc present u updatestatus = do
	uuids <- loggedLocations key
	case (present, u `elem` uuids) of
		(True, False) -> do
				fix InfoPresent
				-- There is no data loss, so do not fail.
				return True
		(False, True) -> do
				fix InfoMissing
				warning $
					"** Based on the location log, " ++ desc
					++ "\n** was expected to be present, " ++
					"but its content is missing."
				return False
		_ -> return True
  where
	fix s = do
		showNote "fixing location log"
		updatestatus s

{- Verifies the associated file records. -}
verifyAssociatedFiles :: Key -> KeyStatus -> FilePath -> Annex Bool
verifyAssociatedFiles key keystatus file = case keystatus of
	KeyUnlocked -> do
		f <- inRepo $ toTopFilePath file
		afs <- Database.Keys.getAssociatedFiles key
		unless (getTopFilePath f `elem` map getTopFilePath afs) $
			Database.Keys.addAssociatedFile key f
		return True
	_ -> return True

verifyWorkTree :: Key -> FilePath -> Annex Bool
verifyWorkTree key file = do
	{- Make sure that a pointer file is replaced with its content,
	 - when the content is available. -}
	mk <- liftIO $ isPointerFile file
	case mk of
		Just k | k == key -> whenM (inAnnex key) $ do
			showNote "fixing worktree content"
			replaceFile file $ \tmp -> 
				ifM (annexThin <$> Annex.getGitConfig)
					( void $ linkFromAnnex key tmp
					, do
						obj <- calcRepo $ gitAnnexLocation key
						void $ checkedCopyFile key obj tmp
						thawContent tmp
					)
			Database.Keys.storeInodeCaches key [file]
		_ -> return ()
	return True

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata, if available.
 -
 - Not checked when a file is unlocked.
 -}
checkKeySize :: Key -> KeyStatus -> Annex Bool
checkKeySize _ KeyUnlocked = return True
checkKeySize key _ = do
	file <- calcRepo $ gitAnnexLocation key
	ifM (liftIO $ doesFileExist file)
		( checkKeySizeOr badContent key file
		, return True
		)

checkKeySizeRemote :: Key -> Remote -> Maybe FilePath -> Annex Bool
checkKeySizeRemote _ _ Nothing = return True
checkKeySizeRemote key remote (Just file) =
	checkKeySizeOr (badContentRemote remote file) key file

checkKeySizeOr :: (Key -> Annex String) -> Key -> FilePath -> Annex Bool
checkKeySizeOr bad key file = case keySize key of
	Nothing -> return True
	Just size -> do
		size' <- liftIO $ getFileSize file
		comparesizes size size'
  where
	comparesizes a b = do
		let same = a == b
		unless same $ badsize a b
		return same
	badsize a b = do
		msg <- bad key
		warning $ concat
			[ "Bad file size ("
			, compareSizes storageUnits True a b
			, "); "
			, msg
			]

{- Runs the backend specific check on a key's content object.
 -
 - When a file is unlocked, it may be a hard link to the object,
 - thus when the user modifies the file, the object will be modified and
 - not pass the check, and we don't want to find an error in this case.
 - So, skip the check if the key is unlocked and modified.
 -}
checkBackend :: Backend -> Key -> KeyStatus -> Annex Bool
checkBackend backend key keystatus = do
	content <- calcRepo $ gitAnnexLocation key
	ifM (pure (isKeyUnlocked keystatus) <&&> (not <$> isUnmodified key content))
		( nocheck
		, checkBackendOr badContent backend key content
		)
  where
	nocheck = return True

checkBackendRemote :: Backend -> Key -> Remote -> Maybe FilePath -> Annex Bool
checkBackendRemote backend key remote = maybe (return True) go
  where
	go file = checkBackendOr (badContentRemote remote file) backend key file

checkBackendOr :: (Key -> Annex String) -> Backend -> Key -> FilePath -> Annex Bool
checkBackendOr bad backend key file =
	checkBackendOr' bad backend key file (return True)

-- The postcheck action is run after the content is verified,
-- in order to detect situations where the file is changed while being
-- verified.
checkBackendOr' :: (Key -> Annex String) -> Backend -> Key -> FilePath -> Annex Bool -> Annex Bool
checkBackendOr' bad backend key file postcheck =
	case Types.Backend.verifyKeyContent backend of
		Nothing -> return True
		Just verifier -> do
			ok <- verifier key file
			ifM postcheck
				( do
					unless ok $ do
						msg <- bad key
						warning $ "Bad file content; " ++ msg
					return ok
				, return True
				)

checkKeyNumCopies :: Key -> AssociatedFile -> NumCopies -> Annex Bool
checkKeyNumCopies key afile numcopies = do
	let file = fromMaybe (key2file key) afile
	locs <- loggedLocations key
	(untrustedlocations, otherlocations) <- trustPartition UnTrusted locs
	(deadlocations, safelocations) <- trustPartition DeadTrusted otherlocations
	let present = NumCopies (length safelocations)
	if present < numcopies
		then ifM (pure (isNothing afile) <&&> checkDead key)
			( do
				showLongNote $ "This key is dead, skipping."
				return True
			, do
				untrusted <- Remote.prettyPrintUUIDs "untrusted" untrustedlocations
				dead <- Remote.prettyPrintUUIDs "dead" deadlocations
				warning $ missingNote file present numcopies untrusted dead
				when (fromNumCopies present == 0 && isNothing afile) $
					showLongNote "(Avoid this check by running: git annex dead --key )"
				return False
			)
		else return True

missingNote :: String -> NumCopies -> NumCopies -> String -> String -> String
missingNote file (NumCopies 0) _ [] dead = 
		"** No known copies exist of " ++ file ++ honorDead dead
missingNote file (NumCopies 0) _ untrusted dead =
		"Only these untrusted locations may have copies of " ++ file ++
		"\n" ++ untrusted ++
		"Back it up to trusted locations with git-annex copy." ++ honorDead dead
missingNote file present needed [] _ =
		"Only " ++ show (fromNumCopies present) ++ " of " ++ show (fromNumCopies needed) ++ 
		" trustworthy copies exist of " ++ file ++
		"\nBack it up with git-annex copy."
missingNote file present needed untrusted dead = 
		missingNote file present needed [] dead ++
		"\nThe following untrusted locations may also have copies: " ++
		"\n" ++ untrusted
	
honorDead :: String -> String
honorDead dead
	| null dead = ""
	| otherwise = "\nThese dead repositories used to have copies\n" ++ dead

{- Bad content is moved aside. -}
badContent :: Key -> Annex String
badContent key = do
	dest <- moveBad key
	return $ "moved to " ++ dest

{- Bad content is dropped from the remote. We have downloaded a copy
 - from the remote to a temp file already (in some cases, it's just a
 - symlink to a file in the remote). To avoid any further data loss,
 - that temp file is moved to the bad content directory unless 
 - the local annex has a copy of the content. -}
badContentRemote :: Remote -> FilePath -> Key -> Annex String
badContentRemote remote localcopy key = do
	bad <- fromRepo gitAnnexBadDir
	let destbad = bad </> key2file key
	movedbad <- ifM (inAnnex key <||> liftIO (doesFileExist destbad))
		( return False
		, do
			createAnnexDirectory (parentDir destbad)
			liftIO $ catchDefaultIO False $
				ifM (isSymbolicLink <$> getSymbolicLinkStatus localcopy)
					( copyFileExternal CopyTimeStamps localcopy destbad
					, do
						moveFile localcopy destbad
						return True
					)
		)

	dropped <- Remote.removeKey remote key
	when dropped $
		Remote.logStatus remote key InfoMissing
	return $ case (movedbad, dropped) of
		(True, True) -> "moved from " ++ Remote.name remote ++
			" to " ++ destbad
		(False, True) -> "dropped from " ++ Remote.name remote
		(_, False) -> "failed to drop from" ++ Remote.name remote

runFsck :: Incremental -> FilePath -> Key -> Annex Bool -> CommandStart
runFsck inc file key a = ifM (needFsck inc key)
	( do
		showStart "fsck" file
		next $ do
			ok <- a
			when ok $
				recordFsckTime inc key
			next $ return ok
	, stop
	)

{- Check if a key needs to be fscked, with support for incremental fscks. -}
needFsck :: Incremental -> Key -> Annex Bool
needFsck (ScheduleIncremental _ _ i) k = needFsck i k
needFsck (ContIncremental h) key = liftIO $ not <$> FsckDb.inDb h key
needFsck _ _ = return True

recordFsckTime :: Incremental -> Key -> Annex ()
recordFsckTime inc key = withFsckDb inc $ \h -> liftIO $ FsckDb.addDb h key

{- Records the start time of an incremental fsck.
 -
 - To guard against time stamp damange (for example, if an annex directory
 - is copied without -a), the fsckstate file contains a time that should
 - be identical to its modification time.
 - (This is not possible to do on Windows, and so the timestamp in
 - the file will only be equal or greater than the modification time.)
 -}
recordStartTime :: UUID -> Annex ()
recordStartTime u = do
	f <- fromRepo (gitAnnexFsckState u)
	createAnnexDirectory $ parentDir f
	liftIO $ do
		nukeFile f
		withFile f WriteMode $ \h -> do
#ifndef mingw32_HOST_OS
			t <- modificationTime <$> getFileStatus f
#else
			t <- getPOSIXTime
#endif
			hPutStr h $ showTime $ realToFrac t
  where
	showTime :: POSIXTime -> String
	showTime = show

resetStartTime :: UUID -> Annex ()
resetStartTime u = liftIO . nukeFile =<< fromRepo (gitAnnexFsckState u)

{- Gets the incremental fsck start time. -}
getStartTime :: UUID -> Annex (Maybe EpochTime)
getStartTime u = do
	f <- fromRepo (gitAnnexFsckState u)
	liftIO $ catchDefaultIO Nothing $ do
		timestamp <- modificationTime <$> getFileStatus f
		let fromstatus = Just (realToFrac timestamp)
		fromfile <- parsePOSIXTime <$> readFile f
		return $ if matchingtimestamp fromfile fromstatus
			then Just timestamp
			else Nothing
  where
	matchingtimestamp fromfile fromstatus =
#ifndef mingw32_HOST_OS
		fromfile == fromstatus
#else
		fromfile >= fromstatus
#endif

data Incremental
	= NonIncremental
	| ScheduleIncremental Duration UUID Incremental
	| StartIncremental FsckDb.FsckHandle 
	| ContIncremental FsckDb.FsckHandle

prepIncremental :: UUID -> Maybe IncrementalOpt -> Annex Incremental
prepIncremental _ Nothing = pure NonIncremental
prepIncremental u (Just StartIncrementalO) = do
	recordStartTime u
	ifM (FsckDb.newPass u)
		( StartIncremental <$> openFsckDb u
		, error "Cannot start a new --incremental fsck pass; another fsck process is already running."
		)
prepIncremental u (Just MoreIncrementalO) =
	ContIncremental <$> openFsckDb u
prepIncremental u (Just (ScheduleIncrementalO delta)) = do
	started <- getStartTime u
	i <- prepIncremental u $ Just $ case started of
		Nothing -> StartIncrementalO
		Just _ -> MoreIncrementalO
	return (ScheduleIncremental delta u i)

cleanupIncremental :: Incremental -> Annex ()
cleanupIncremental (ScheduleIncremental delta u i) = do
	v <- getStartTime u
	case v of
		Nothing -> noop
		Just started -> do
			now <- liftIO getPOSIXTime
			when (now - realToFrac started >= durationToPOSIXTime delta) $
				resetStartTime u
	cleanupIncremental i
cleanupIncremental _ = return ()

openFsckDb :: UUID -> Annex FsckDb.FsckHandle
openFsckDb u = do
	h <- FsckDb.openDb u
	Annex.addCleanup FsckCleanup $
		FsckDb.closeDb h
	return h

withFsckDb :: Incremental -> (FsckDb.FsckHandle -> Annex ()) -> Annex ()
withFsckDb (ContIncremental h) a = a h
withFsckDb (StartIncremental h) a = a h
withFsckDb NonIncremental _ = noop
withFsckDb (ScheduleIncremental _ _ i) a = withFsckDb i a

data KeyStatus = KeyLocked | KeyUnlocked | KeyMissing

isKeyUnlocked :: KeyStatus -> Bool
isKeyUnlocked KeyUnlocked = True
isKeyUnlocked KeyLocked = False
isKeyUnlocked KeyMissing = False

getKeyStatus :: Key -> Annex KeyStatus
getKeyStatus key = catchDefaultIO KeyMissing $ do
	unlocked <- not . null <$> Database.Keys.getAssociatedFiles key
	return $ if unlocked then KeyUnlocked else KeyLocked

getKeyFileStatus :: Key -> FilePath -> Annex KeyStatus
getKeyFileStatus key file = do
	s <- getKeyStatus key
	case s of
		KeyLocked -> catchDefaultIO KeyLocked $
			ifM (isJust <$> isAnnexLink file)
				( return KeyLocked
				, return KeyUnlocked
				)
		_ -> return s
