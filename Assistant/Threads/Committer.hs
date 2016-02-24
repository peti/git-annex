{- git-annex assistant commit thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Threads.Committer where

import Assistant.Common
import Assistant.Changes
import Assistant.Types.Changes
import Assistant.Commits
import Assistant.Alert
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.Drop
import Logs.Transfer
import Logs.Location
import qualified Annex.Queue
import qualified Git.LsFiles
import Utility.ThreadScheduler
import qualified Utility.Lsof as Lsof
import qualified Utility.DirWatcher as DirWatcher
import Types.KeySource
import Config
import Annex.Content
import Annex.Ingest
import Annex.Link
import Annex.CatFile
import Annex.InodeSentinal
import Annex.Version
import qualified Annex
import Utility.InodeCache
import qualified Database.Keys
import qualified Command.Sync
import qualified Git.Branch

import Data.Time.Clock
import Data.Tuple.Utils
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either
import Control.Concurrent

{- This thread makes git commits at appropriate times. -}
commitThread :: NamedThread
commitThread = namedThread "Committer" $ do
	havelsof <- liftIO $ inPath "lsof"
	delayadd <- liftAnnex $
		maybe delayaddDefault (return . Just . Seconds)
			=<< annexDelayAdd <$> Annex.getGitConfig
	msg <- liftAnnex Command.Sync.commitMsg
	waitChangeTime $ \(changes, time) -> do
		readychanges <- handleAdds havelsof delayadd $
			simplifyChanges changes
		if shouldCommit False time (length readychanges) readychanges
			then do
				debug
					[ "committing"
					, show (length readychanges)
					, "changes"
					]
				void $ alertWhile commitAlert $
					liftAnnex $ commitStaged msg
				recordCommit
				let numchanges = length readychanges
				mapM_ checkChangeContent readychanges
				return numchanges
			else do
				refill readychanges
				return 0

refill :: [Change] -> Assistant ()	
refill [] = noop
refill cs = do
	debug ["delaying commit of", show (length cs), "changes"]
	refillChanges cs

{- Wait for one or more changes to arrive to be committed, and then
 - runs an action to commit them. If more changes arrive while this is
 - going on, they're handled intelligently, batching up changes into
 - large commits where possible, doing rename detection, and
 - commiting immediately otherwise. -}
waitChangeTime :: (([Change], UTCTime) -> Assistant Int) -> Assistant ()
waitChangeTime a = waitchanges 0
  where
	waitchanges lastcommitsize = do
		-- Wait one one second as a simple rate limiter.
		liftIO $ threadDelaySeconds (Seconds 1)
		-- Now, wait until at least one change is available for
		-- processing.
		cs <- getChanges
		handlechanges cs lastcommitsize
	handlechanges changes lastcommitsize = do
		let len = length changes
		-- See if now's a good time to commit.
		now <- liftIO getCurrentTime
		scanning <- not . scanComplete <$> getDaemonStatus
		case (lastcommitsize >= maxCommitSize, shouldCommit scanning now len changes, possiblyrename changes) of
			(True, True, _)
				| len > maxCommitSize -> 
					a (changes, now) >>= waitchanges
				| otherwise -> aftermaxcommit changes
			(_, True, False) ->
				a (changes, now) >>= waitchanges
			(_, True, True) -> do
				morechanges <- getrelatedchanges changes
				a (changes ++ morechanges, now) >>= waitchanges
			_ -> do
				refill changes
				waitchanges lastcommitsize
	
	{- Did we perhaps only get one of the AddChange and RmChange pair
	 - that make up a file rename? Or some of the pairs that make up 
	 - a directory rename?
	 -}
	possiblyrename = all renamepart

	renamepart (PendingAddChange _ _) = True
	renamepart c = isRmChange c

	{- Gets changes related to the passed changes, without blocking
	 - very long.
	 -
	 - If there are multiple RmChanges, this is probably a directory
	 - rename, in which case it may be necessary to wait longer to get
	 - all the Changes involved.
	 -}
	getrelatedchanges oldchanges
		| length (filter isRmChange oldchanges) > 1 =
			concat <$> getbatchchanges []
		| otherwise = do
			liftIO humanImperceptibleDelay
			getAnyChanges
	getbatchchanges cs = do
		liftIO $ threadDelay $ fromIntegral $ oneSecond `div` 10
		cs' <- getAnyChanges
		if null cs'
			then return cs
			else getbatchchanges (cs':cs)

	{- The last commit was maximum size, so it's very likely there
	 - are more changes and we'd like to ensure we make another commit
	 - of maximum size if possible.
	 -
	 - But, it can take a while for the Watcher to wake back up
	 - after a commit. It can get blocked by another thread
	 - that is using the Annex state, such as a git-annex branch
	 - commit. Especially after such a large commit, this can
	 - take several seconds. When this happens, it defeats the
	 - normal commit batching, which sees some old changes the
	 - Watcher found while the commit was being prepared, and sees
	 - no recent ones, and wants to commit immediately.
	 -
	 - All that we need to do, then, is wait for the Watcher to
	 - wake up, and queue up one more change.
	 -
	 - However, it's also possible that we're at the end of changes for
	 - now. So to avoid waiting a really long time before committing
	 - those changes we have, poll for up to 30 seconds, and then
	 - commit them.
	 -
	 - Also, try to run something in Annex, to ensure we block
	 - longer if the Annex state is indeed blocked.
	 -}
	aftermaxcommit oldchanges = loop (30 :: Int)
	  where
		loop 0 = continue oldchanges
		loop n = do
			liftAnnex noop -- ensure Annex state is free
			liftIO $ threadDelaySeconds (Seconds 1)
			changes <- getAnyChanges
			if null changes
				then loop (n - 1)
				else continue (oldchanges ++ changes)
		continue cs
			| null cs = waitchanges 0
			| otherwise = handlechanges cs 0

isRmChange :: Change -> Bool
isRmChange (Change { changeInfo = i }) | i == RmChange = True
isRmChange _ = False

{- An amount of time that is hopefully imperceptably short for humans,
 - while long enough for a computer to get some work done. 
 - Note that 0.001 is a little too short for rename change batching to
 - work. -}
humanImperceptibleInterval :: NominalDiffTime
humanImperceptibleInterval = 0.01

humanImperceptibleDelay :: IO ()
humanImperceptibleDelay = threadDelay $
	truncate $ humanImperceptibleInterval * fromIntegral oneSecond

maxCommitSize :: Int
maxCommitSize = 5000

{- Decide if now is a good time to make a commit.
 - Note that the list of changes has a random order.
 -
 - Current strategy: If there have been 10 changes within the past second,
 - a batch activity is taking place, so wait for later.
 -}
shouldCommit :: Bool -> UTCTime -> Int -> [Change] -> Bool
shouldCommit scanning now len changes
	| scanning = len >= maxCommitSize
	| len == 0 = False
	| len >= maxCommitSize = True
	| length recentchanges < 10 = True
	| otherwise = False -- batch activity
  where
	thissecond c = timeDelta c <= 1
	recentchanges = filter thissecond changes
	timeDelta c = now `diffUTCTime` changeTime c

commitStaged :: String -> Annex Bool
commitStaged msg = do
	{- This could fail if there's another commit being made by
	 - something else. -}
	v <- tryNonAsync Annex.Queue.flush
	case v of
		Left _ -> return False
		Right _ -> do
			ok <- Command.Sync.commitStaged Git.Branch.AutomaticCommit msg
			when ok $
				Command.Sync.updateSyncBranch =<< inRepo Git.Branch.current
			return ok

{- OSX needs a short delay after a file is added before locking it down,
 - as pasting a file seems to try to set file permissions or otherwise
 - access the file after closing it. -}
delayaddDefault :: Annex (Maybe Seconds)
#ifdef darwin_HOST_OS
delayaddDefault = ifM versionSupportsUnlockedPointers
	( return Nothing
	, return $ Just $ Seconds 1
	)
#else
delayaddDefault = return Nothing
#endif

{- If there are PendingAddChanges, or InProcessAddChanges, the files
 - have not yet actually been added to the annex, and that has to be done
 - now, before committing.
 -
 - Deferring the adds to this point causes batches to be bundled together,
 - which allows faster checking with lsof that the files are not still open
 - for write by some other process, and faster checking with git-ls-files
 - that the files are not already checked into git.
 -
 - When a file is added in locked mode, Inotify will notice the new symlink.
 - So this waits for additional Changes to arrive, so that the symlink has
 - hopefully been staged before returning, and will be committed immediately.
 - (OTOH, for kqueue, eventsCoalesce, so instead the symlink is directly
 - created and staged.)
 -
 - Returns a list of all changes that are ready to be committed.
 - Any pending adds that are not ready yet are put back into the ChangeChan,
 - where they will be retried later.
 -}
handleAdds :: Bool -> Maybe Seconds -> [Change] -> Assistant [Change]
handleAdds havelsof delayadd cs = returnWhen (null incomplete) $ do
	let (pending, inprocess) = partition isPendingAddChange incomplete
	unlocked <- liftAnnex versionSupportsUnlockedPointers
	let lockingfiles = not unlocked
	let lockdownconfig = LockDownConfig
		{ lockingFile = lockingfiles
		, hardlinkFileTmp = True
		}
	(pending', cleanup) <- if unlocked
		then return (pending, noop)
		else findnew pending
	(postponed, toadd) <- partitionEithers
		<$> safeToAdd lockdownconfig havelsof delayadd pending' inprocess
	cleanup

	unless (null postponed) $
		refillChanges postponed

	returnWhen (null toadd) $ do
		added <- addaction toadd $
			catMaybes <$>
				if not lockingfiles
					then addunlocked toadd
					else forM toadd (add lockdownconfig)
		if DirWatcher.eventsCoalesce || null added || unlocked
			then return $ added ++ otherchanges
			else do
				r <- handleAdds havelsof delayadd =<< getChanges
				return $ r ++ added ++ otherchanges
  where
	(incomplete, otherchanges) = partition (\c -> isPendingAddChange c || isInProcessAddChange c) cs
	
	-- Find files that are actually new, and not unlocked annexed
	-- files. The ls-files is run on a batch of files.
	findnew [] = return ([], noop)
	findnew pending@(exemplar:_) = do
		let segments = segmentXargsUnordered $ map changeFile pending
		rs <- liftAnnex $ forM segments $ \fs ->
			inRepo (Git.LsFiles.notInRepo False fs)
		let (newfiles, cleanup) = foldl'
			(\(l1, a1) (l2, a2) -> (l1 ++ l2, a1 >> a2))
			([], return True) rs
		-- note: timestamp info is lost here
		let ts = changeTime exemplar
		return (map (PendingAddChange ts) newfiles, void $ liftIO cleanup)

	returnWhen c a
		| c = return otherchanges
		| otherwise = a

	add :: LockDownConfig -> Change -> Assistant (Maybe Change)
	add lockdownconfig change@(InProcessAddChange { lockedDown = ld }) = 
		catchDefaultIO Nothing <~> doadd
	  where
	  	ks = keySource ld
		doadd = sanitycheck ks $ do
			(mkey, mcache) <- liftAnnex $ do
				showStart "add" $ keyFilename ks
				ingest $ Just $ LockedDown lockdownconfig ks
			maybe (failedingest change) (done change mcache $ keyFilename ks) mkey
	add _ _ = return Nothing

	{- Avoid overhead of re-injesting a renamed unlocked file, by
	 - examining the other Changes to see if a removed file has the
	 - same InodeCache as the new file. If so, we can just update
	 - bookkeeping, and stage the file in git.
	 -}
	addunlocked :: [Change] -> Assistant [Maybe Change]
	addunlocked toadd = do
		ct <- liftAnnex compareInodeCachesWith
		m <- liftAnnex $ removedKeysMap ct cs
		delta <- liftAnnex getTSDelta
		let cfg = LockDownConfig
			{ lockingFile = False
			, hardlinkFileTmp = True
			}
		if M.null m
			then forM toadd (add cfg)
			else forM toadd $ \c -> do
				mcache <- liftIO $ genInodeCache (changeFile c) delta
				case mcache of
					Nothing -> add cfg c
					Just cache ->
						case M.lookup (inodeCacheToKey ct cache) m of
							Nothing -> add cfg c
							Just k -> fastadd c k

	fastadd :: Change -> Key -> Assistant (Maybe Change)
	fastadd change key = do
		let source = keySource $ lockedDown change
		liftAnnex $ finishIngestUnlocked key source
		done change Nothing (keyFilename source) key

	removedKeysMap :: InodeComparisonType -> [Change] -> Annex (M.Map InodeCacheKey Key)
	removedKeysMap ct l = do
		mks <- forM (filter isRmChange l) $ \c ->
			catKeyFile $ changeFile c
		M.fromList . concat <$> mapM mkpairs (catMaybes mks)
	  where
		mkpairs k = map (\c -> (inodeCacheToKey ct c, k))
			<$> Database.Keys.getInodeCaches k

	failedingest change = do
		refill [retryChange change]
		liftAnnex showEndFail
		return Nothing

	done change mcache file key = liftAnnex $ do
		logStatus key InfoPresent
		ifM versionSupportsUnlockedPointers
			( stagePointerFile file =<< hashPointerFile key
			, do
				link <- makeLink file key mcache
				whenM (pure DirWatcher.eventsCoalesce) $
					stageSymlink file =<< hashSymlink link
			)
		showEndOk
		return $ Just $ finishedChange change key

	{- Check that the keysource's keyFilename still exists,
	 - and is still a hard link to its contentLocation,
	 - before ingesting it. -}
	sanitycheck keysource a = do
		fs <- liftIO $ getSymbolicLinkStatus $ keyFilename keysource
		ks <- liftIO $ getSymbolicLinkStatus $ contentLocation keysource
		if deviceID ks == deviceID fs && fileID ks == fileID fs
			then a
			else do
				-- remove the hard link
				when (contentLocation keysource /= keyFilename keysource) $
					void $ liftIO $ tryIO $ removeFile $ contentLocation keysource
				return Nothing

	{- Shown an alert while performing an action to add a file or
	 - files. When only a few files are added, their names are shown
	 - in the alert. When it's a batch add, the number of files added
	 - is shown.
	 -
	 - Add errors tend to be transient and will be
	 - automatically dealt with, so the alert is always told
	 - the add succeeded.
	 -}
	addaction [] a = a
	addaction toadd a = alertWhile' (addFileAlert $ map changeFile toadd) $
		(,) 
			<$> pure True
			<*> a

{- Files can Either be Right to be added now,
 - or are unsafe, and must be Left for later.
 -
 - Check by running lsof on the repository.
 -}
safeToAdd :: LockDownConfig -> Bool -> Maybe Seconds -> [Change] -> [Change] -> Assistant [Either Change Change]
safeToAdd _ _ _ [] [] = return []
safeToAdd lockdownconfig havelsof delayadd pending inprocess = do
	maybe noop (liftIO . threadDelaySeconds) delayadd
	liftAnnex $ do
		lockeddown <- forM pending $ lockDown lockdownconfig . changeFile
		let inprocess' = inprocess ++ mapMaybe mkinprocess (zip pending lockeddown)
		openfiles <- if havelsof
			then S.fromList . map fst3 . filter openwrite <$>
				findopenfiles (map (keySource . lockedDown) inprocess')
			else pure S.empty
		let checked = map (check openfiles) inprocess'

		{- If new events are received when files are closed,
		 - there's no need to retry any changes that cannot
		 - be done now. -}
		if DirWatcher.closingTracked
			then do
				mapM_ canceladd $ lefts checked
				allRight $ rights checked
			else return checked
  where
	check openfiles change@(InProcessAddChange { lockedDown = ld })
		| S.member (contentLocation (keySource ld)) openfiles = Left change
	check _ change = Right change

	mkinprocess (c, Just ld) = Just InProcessAddChange
		{ changeTime = changeTime c
		, lockedDown = ld
		}
	mkinprocess (_, Nothing) = Nothing

	canceladd (InProcessAddChange { lockedDown = ld }) = do
		let ks = keySource ld
		warning $ keyFilename ks
			++ " still has writers, not adding"
		-- remove the hard link
		when (contentLocation ks /= keyFilename ks) $
			void $ liftIO $ tryIO $ removeFile $ contentLocation ks
	canceladd _ = noop

	openwrite (_file, mode, _pid)
		| mode == Lsof.OpenWriteOnly = True
		| mode == Lsof.OpenReadWrite = True
		| mode == Lsof.OpenUnknown = True
		| otherwise = False

	allRight = return . map Right

	{- Normally the KeySources are locked down inside the temp directory,
	 - so can just lsof that, which is quite efficient.
	 -
	 - In crippled filesystem mode, there is no lock down, so must run lsof
	 - on each individual file.
	 -}
	findopenfiles keysources = ifM crippledFileSystem
		( liftIO $ do
			let segments = segmentXargsUnordered $ map keyFilename keysources
			concat <$> forM segments (\fs -> Lsof.query $ "--" : fs)
		, do
			tmpdir <- fromRepo gitAnnexTmpMiscDir
			liftIO $ Lsof.queryDir tmpdir
		)

{- After a Change is committed, queue any necessary transfers or drops
 - of the content of the key.
 -
 - This is not done during the startup scan, because the expensive
 - transfer scan does the same thing then.
 -}
checkChangeContent :: Change -> Assistant ()
checkChangeContent change@(Change { changeInfo = i }) =
	case changeInfoKey i of
		Nothing -> noop
		Just k -> whenM (scanComplete <$> getDaemonStatus) $ do
			present <- liftAnnex $ inAnnex k
			void $ if present
				then queueTransfers "new file created" Next k (Just f) Upload
				else queueTransfers "new or renamed file wanted" Next k (Just f) Download
			handleDrops "file renamed" present k (Just f) []
  where
	f = changeFile change
checkChangeContent _ = noop
