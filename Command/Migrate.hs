{- git-annex command
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Migrate where

import Command
import Backend
import Types.Backend (canUpgradeKey, fastMigrate)
import Types.KeySource
import Annex.Content
import qualified Command.ReKey
import qualified Command.Fsck
import qualified Annex
import Logs.MetaData
import Logs.Web
import qualified Remote

cmd :: Command
cmd = withGlobalOptions annexedMatchingOptions $
	command "migrate" SectionUtility 
		"switch data to different backend"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withFilesInGit $ whenAnnexed start

start :: FilePath -> Key -> CommandStart
start file key = do
	forced <- Annex.getState Annex.force
	v <- Backend.getBackend file key
	case v of
		Nothing -> stop
		Just oldbackend -> do
			exists <- inAnnex key
			newbackend <- choosebackend =<< chooseBackend file
			if (newbackend /= oldbackend || upgradableKey oldbackend key || forced) && exists
				then do
					showStart "migrate" file
					next $ perform file key oldbackend newbackend
				else stop
  where
	choosebackend Nothing = Prelude.head <$> orderedList
	choosebackend (Just backend) = return backend

{- Checks if a key is upgradable to a newer representation.
 - 
 - Reasons for migration:
 -  - Ideally, all keys have file size metadata. Old keys may not.
 -  - Something has changed in the backend, such as a bug fix.
 -}
upgradableKey :: Backend -> Key -> Bool
upgradableKey backend key = isNothing (keySize key) || backendupgradable
  where
	backendupgradable = maybe False (\a -> a key) (canUpgradeKey backend)

{- Store the old backend's key in the new backend
 - The old backend's key is not dropped from it, because there may
 - be other files still pointing at that key.
 -
 - To ensure that the data we have for the old key is valid, it's
 - fscked here. First we generate the new key. This ensures that the
 - data cannot get corrupted after the fsck but before the new key is
 - generated.
 -}
perform :: FilePath -> Key -> Backend -> Backend -> CommandPerform
perform file oldkey oldbackend newbackend = go =<< genkey
  where
	go Nothing = stop
	go (Just (newkey, knowngoodcontent))
		| knowngoodcontent = finish newkey
		| otherwise = stopUnless checkcontent $ finish newkey
	checkcontent = Command.Fsck.checkBackend oldbackend oldkey Command.Fsck.KeyLocked
	finish newkey = ifM (Command.ReKey.linkKey file oldkey newkey)
		( do
			copyMetaData oldkey newkey
			-- If the old key had some associated urls, record them for
			-- the new key as well.
			urls <- getUrls oldkey
			forM_ urls $ \url -> do
				r <- Remote.claimingUrl url
				setUrlPresent (Remote.uuid r) newkey url
			next $ Command.ReKey.cleanup file oldkey newkey
		, error "failed"
		)
	genkey = case maybe Nothing (\fm -> fm oldkey newbackend (Just file)) (fastMigrate oldbackend) of
		Just newkey -> return $ Just (newkey, True)
		Nothing -> do
			content <- calcRepo $ gitAnnexLocation oldkey
			let source = KeySource
				{ keyFilename = file
				, contentLocation = content
				, inodeCache = Nothing
				}
			v <- genKey source (Just newbackend)
			return $ case v of
				Just (newkey, _) -> Just (newkey, False)
				_ -> Nothing
