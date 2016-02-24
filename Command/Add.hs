{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Add where

import Command
import Annex.Ingest
import Logs.Location
import Annex.Content
import qualified Annex
import qualified Annex.Queue
import qualified Database.Keys
import Config
import Annex.FileMatcher
import Annex.Version
import Annex.InodeSentinal

cmd :: Command
cmd = notBareRepo $ withGlobalOptions (jobsOption : jsonOption : fileMatchingOptions) $
	command "add" SectionCommon "add files to annex"
		paramPaths (seek <$$> optParser)

data AddOptions = AddOptions
	{ addThese :: CmdParams
	, includeDotFiles :: Bool
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser AddOptions
optParser desc = AddOptions
	<$> cmdParams desc
	<*> switch
		( long "include-dotfiles"
		<> help "don't skip dotfiles"
		)
	<*> parseBatchOption

{- Add acts on both files not checked into git yet, and unlocked files.
 -
 - In direct mode, it acts on any files that have changed. -}
seek :: AddOptions -> CommandSeek
seek o = allowConcurrentOutput $ do
	matcher <- largeFilesMatcher
	let gofile file = ifM (checkFileMatcher matcher file <||> Annex.getState Annex.force)
		( start file
		, ifM (annexAddSmallFiles <$> Annex.getGitConfig)
			( startSmall file
			, stop
			)
		)
	case batchOption o of
		Batch -> batchFiles gofile
		NoBatch -> do
			let go a = a gofile (addThese o)
			go (withFilesNotInGit (not $ includeDotFiles o))
			ifM versionSupportsUnlockedPointers
				( go withFilesMaybeModified
				, go withFilesOldUnlocked
				)

{- Pass file off to git-add. -}
startSmall :: FilePath -> CommandStart
startSmall file = do
	showStart "add" file
	next $ next $ addSmall file

addSmall :: FilePath -> Annex Bool
addSmall file = do
	showNote "non-large file; adding content to git repository"
	addFile file

addFile :: FilePath -> Annex Bool
addFile file = do
	ps <- forceParams
	Annex.Queue.addCommand "add" (ps++[Param "--"]) [file]
	return True

start :: FilePath -> CommandStart
start file = ifAnnexed file addpresent add
  where
	add = do
		ms <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		case ms of
			Nothing -> stop
			Just s 
				| not (isRegularFile s) && not (isSymbolicLink s) -> stop
				| otherwise -> do
					showStart "add" file
					next $ if isSymbolicLink s
						then next $ addFile file
						else perform file
	addpresent key = ifM versionSupportsUnlockedPointers
		( do
			ms <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
			case ms of
				Just s | isSymbolicLink s -> fixuplink key
				_ -> ifM (sameInodeCache file =<< Database.Keys.getInodeCaches key)
						( stop, add )
		, fixuplink key
		)
	fixuplink key = do
		-- the annexed symlink is present but not yet added to git
		showStart "add" file
		liftIO $ removeFile file
		next $ next $ do
			addLink file key Nothing
			cleanup key =<< inAnnex key

perform :: FilePath -> CommandPerform
perform file = do
	lockingfile <- not <$> addUnlocked
	let cfg = LockDownConfig
		{ lockingFile = lockingfile
		, hardlinkFileTmp = True
		}
	lockDown cfg file >>= ingestAdd >>= finish
  where
	finish (Just key) = next $ cleanup key True
	finish Nothing = stop

cleanup :: Key -> Bool -> CommandCleanup
cleanup key hascontent = do
	maybeShowJSON [("key", key2file key)]
	when hascontent $
		logStatus key InfoPresent
	return True
