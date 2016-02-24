{- git-annex fuzz generator
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FuzzTest where

import Command
import qualified Annex
import qualified Git.Config
import Config
import Utility.ThreadScheduler
import Utility.DiskFree

import Data.Time.Clock
import System.Random (getStdRandom, random, randomR)
import Test.QuickCheck
import Control.Concurrent

cmd :: Command
cmd = notBareRepo $
	command "fuzztest" SectionTesting
		"generates fuzz test files"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	guardTest
	logf <- fromRepo gitAnnexFuzzTestLogFile
	showStart "fuzztest" logf
	logh <-liftIO $ openFile logf WriteMode
	void $ forever $ fuzz logh
	stop

guardTest :: Annex ()
guardTest = unlessM (fromMaybe False . Git.Config.isTrue <$> getConfig key "") $
	error $ unlines
		[ "Running fuzz tests *writes* to and *deletes* files in"
		, "this repository, and pushes those changes to other"
		, "repositories! This is a developer tool, not something"
		, "to play with."
		, ""
		, "Refusing to run fuzz tests, since " ++ keyname ++ " is not set!"
		]
  where
	key = annexConfig "eat-my-repository"
	(ConfigKey keyname) = key


fuzz :: Handle -> Annex ()
fuzz logh = do
	fuzzer <- genFuzzAction
	record logh $ flip Started fuzzer
	result <- tryNonAsync $ runFuzzAction fuzzer
	record logh $ flip Finished $
		either (const False) (const True) result

record :: Handle -> (UTCTime -> TimeStampedFuzzAction) -> Annex ()
record h tmpl = liftIO $ do
	now <- getCurrentTime
	let s = show $ tmpl now
	print s
	hPrint h s
	hFlush h

{- Delay for either a fraction of a second, or a few seconds, or up
 - to 1 minute.
 -
 - The MinutesDelay is used as an opportunity to do housekeeping tasks.
 -}
randomDelay :: Delay -> Annex ()
randomDelay TinyDelay = liftIO $
	threadDelay =<< getStdRandom (randomR (10000, 1000000))
randomDelay SecondsDelay = liftIO $ 
	threadDelaySeconds =<< Seconds <$> getStdRandom (randomR (1, 10))
randomDelay MinutesDelay = do
	liftIO $ threadDelaySeconds =<< Seconds <$> getStdRandom (randomR (1, 60))
	reserve <- annexDiskReserve <$> Annex.getGitConfig
	free <- liftIO $ getDiskFree "."
	case free of
		Just have | have < reserve -> do
			warning "Low disk space; fuzz test paused."
			liftIO $ threadDelaySeconds (Seconds 60)
			randomDelay MinutesDelay
		_  -> noop

data Delay
	= TinyDelay
	| SecondsDelay
	| MinutesDelay
	deriving (Read, Show, Eq)

instance Arbitrary Delay where
	arbitrary = elements [TinyDelay, SecondsDelay, MinutesDelay]

data FuzzFile = FuzzFile FilePath
	deriving (Read, Show, Eq)

data FuzzDir = FuzzDir FilePath
	deriving (Read, Show, Eq)

instance Arbitrary FuzzFile where
	arbitrary = FuzzFile <$> arbitrary

instance Arbitrary FuzzDir where
	arbitrary = FuzzDir <$> arbitrary

class ToFilePath a where
	toFilePath :: a -> FilePath

instance ToFilePath FuzzFile where
	toFilePath (FuzzFile f) = f

instance ToFilePath FuzzDir where
	toFilePath (FuzzDir d) = d

isFuzzFile :: FilePath -> Bool
isFuzzFile f = "fuzzfile_" `isPrefixOf` takeFileName f

isFuzzDir :: FilePath -> Bool
isFuzzDir d = "fuzzdir_" `isPrefixOf` d

mkFuzzFile :: FilePath -> [FuzzDir] -> FuzzFile
mkFuzzFile file dirs = FuzzFile $ joinPath (map toFilePath dirs) </> ("fuzzfile_" ++ file)

mkFuzzDir :: Int -> FuzzDir
mkFuzzDir n = FuzzDir $ "fuzzdir_" ++ show n

{- File is placed inside a directory hierarchy up to 4 subdirectories deep. -}
genFuzzFile :: IO FuzzFile
genFuzzFile = do
	n <- getStdRandom $ randomR (0, 4)
	dirs <- replicateM n genFuzzDir
	file <- show <$> (getStdRandom random :: IO Int)
	return $ mkFuzzFile file dirs

{- Only 16 distinct subdirectories are used. When nested 4 deep, this
 - yields 69904 total directories max, which is below the default Linux
 - inotify limit of 81920. The goal is not to run the assistant out of
 - inotify descriptors. -}
genFuzzDir :: IO FuzzDir
genFuzzDir = mkFuzzDir <$> (getStdRandom (randomR (1,16)) :: IO Int)

data TimeStampedFuzzAction 
	= Started UTCTime FuzzAction
	| Finished UTCTime Bool
	deriving (Read, Show)

data FuzzAction
	= FuzzAdd FuzzFile
	| FuzzDelete FuzzFile
	| FuzzMove FuzzFile FuzzFile
	| FuzzDeleteDir FuzzDir
	| FuzzMoveDir FuzzDir FuzzDir
	| FuzzPause Delay
	deriving (Read, Show, Eq)

instance Arbitrary FuzzAction where
	arbitrary = frequency
		[ (50, FuzzAdd <$> arbitrary)
		, (50, FuzzDelete <$> arbitrary)
		, (10, FuzzMove <$> arbitrary <*> arbitrary)
		, (10, FuzzDeleteDir <$> arbitrary)
		, (10, FuzzMoveDir <$> arbitrary <*> arbitrary)
		, (10, FuzzPause <$> arbitrary)
		]

runFuzzAction :: FuzzAction -> Annex ()
runFuzzAction (FuzzAdd (FuzzFile f)) = liftIO $ do
	createDirectoryIfMissing True $ parentDir f
	n <- getStdRandom random :: IO Int
	writeFile f $ show n ++ "\n"
runFuzzAction (FuzzDelete (FuzzFile f)) = liftIO $ nukeFile f
runFuzzAction (FuzzMove (FuzzFile src) (FuzzFile dest)) = liftIO $
	rename src dest
runFuzzAction (FuzzDeleteDir (FuzzDir d)) = liftIO $
	removeDirectoryRecursive d
runFuzzAction (FuzzMoveDir (FuzzDir src) (FuzzDir dest)) = liftIO $
	rename src dest
runFuzzAction (FuzzPause d) = randomDelay d

genFuzzAction :: Annex FuzzAction
genFuzzAction = do
	tmpl <- liftIO $ Prelude.head <$> sample' (arbitrary :: Gen FuzzAction)
	-- Fix up template action to make sense in the current repo tree.
	case tmpl of
		FuzzAdd _ -> do
			f <- liftIO newFile
			maybe genFuzzAction (return . FuzzAdd) f
		FuzzDelete _ -> do
			f <- liftIO $ existingFile 0 ""
			maybe genFuzzAction (return . FuzzDelete) f
		FuzzMove _ _ -> do
			src <- liftIO $ existingFile 0 ""
			dest <- liftIO newFile
			case (src, dest) of
				(Just s, Just d) -> return $ FuzzMove s d
				_ -> genFuzzAction
		FuzzMoveDir _ _ -> do
			md <- liftIO existingDir
			case md of
				Nothing -> genFuzzAction
				Just d -> do
					newd <- liftIO $ newDir (parentDir $ toFilePath d)
					maybe genFuzzAction (return . FuzzMoveDir d) newd
		FuzzDeleteDir _ -> do
			d <- liftIO existingDir
			maybe genFuzzAction (return . FuzzDeleteDir) d
		FuzzPause _ -> return tmpl

existingFile :: Int -> FilePath -> IO (Maybe FuzzFile)
existingFile 0 _ = return Nothing
existingFile n top = do
	dir <- existingDirIncludingTop
	contents <- catchDefaultIO [] (getDirectoryContents dir)
	let files = filter isFuzzFile contents
	if null files
		then do
			let dirs = filter isFuzzDir contents
			if null dirs
				then return Nothing
				else do
					i <- getStdRandom $ randomR (0, length dirs - 1)
					existingFile (n - 1) (top </> dirs !! i)
		else do
			i <- getStdRandom $ randomR (0, length files - 1)
			return $ Just $ FuzzFile $ top </> dir </> files !! i

existingDirIncludingTop :: IO FilePath
existingDirIncludingTop = do
	dirs <- filter isFuzzDir <$> getDirectoryContents "."
	if null dirs
		then return "."
		else do
			n <- getStdRandom $ randomR (0, length dirs)
			return $ ("." : dirs) !! n

existingDir :: IO (Maybe FuzzDir)
existingDir = do
	d <- existingDirIncludingTop
	return $ if isFuzzDir d
		then Just $ FuzzDir d
		else Nothing

newFile :: IO (Maybe FuzzFile)
newFile = go (100 :: Int)
  where
	go 0 = return Nothing
	go n = do
		f <- genFuzzFile
		ifM (doesnotexist (toFilePath f))
			( return $ Just f
			, go (n - 1)
			)

newDir :: FilePath -> IO (Maybe FuzzDir)
newDir parent = go (100 :: Int)
  where
	go 0 = return Nothing
	go n = do
		(FuzzDir d) <- genFuzzDir
		ifM (doesnotexist (parent </> d))
			( return $ Just $ FuzzDir d
			, go (n - 1)
			)

doesnotexist :: FilePath -> IO Bool
doesnotexist f = isNothing <$> catchMaybeIO (getSymbolicLinkStatus f)
