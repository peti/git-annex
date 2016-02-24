{- git-annex v5 -> v6 upgrade support
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V5 where

import Annex.Common
import Config
import Annex.InodeSentinal
import Annex.Link
import Annex.Content
import Annex.CatFile
import qualified Database.Keys
import qualified Git
import qualified Git.LsFiles
import qualified Git.Branch
import Git.FilePath
import Git.FileMode
import Utility.InodeCache

upgrade :: Bool -> Annex Bool
upgrade automatic = do
	unless automatic $
		showAction "v5 to v6"
	Database.Keys.scanAssociatedFiles
	configureSmudgeFilter
	-- Inode sentinal file was only used in direct mode and when
	-- locking down files as they were added. In v6, it's used more
	-- extensively, so make sure it exists, since old repos that didn't
	-- use direct mode may not have created it.
	createInodeSentinalFile True
	return True

upgradeDirectCommit :: Bool -> String -> Annex ()
upgradeDirectCommit automatic msg = 
	void $ inRepo $ Git.Branch.commitCommand commitmode
		[ Param "-m"
		, Param msg
		]
  where
	commitmode = if automatic then Git.Branch.AutomaticCommit else Git.Branch.ManualCommit

{- Walk work tree from top and convert all annex symlinks to pointer files,
 - staging them in the index, and updating the work tree files with
 - either the content of the object, or the pointer file content. -}
upgradeDirectWorkTree :: Annex ()
upgradeDirectWorkTree = do
	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.stagedDetails [top]
	forM_ l go
	void $ liftIO clean
  where
	go (f, Just _sha, Just mode) | isSymLink mode = do
		-- Cannot use lookupFile here, as we're in between direct
		-- mode and v6.
		mk <- catKeyFile f
		case mk of
			Nothing -> noop
			Just k -> do
				ifM (isJust <$> getAnnexLinkTarget f)
					( writepointer f k
					, fromdirect f k
					)
				stagePointerFile f =<< hashPointerFile k
				Database.Keys.addAssociatedFile k
					=<< inRepo (toTopFilePath f)
				return ()
	go _ = noop

	fromdirect f k = do
		-- If linkToAnnex fails for some reason, the work tree file
		-- still has the content; the annex object file is just
		-- not populated with it. Since the work tree file
		-- is recorded as an associated file, things will still
		-- work that way, it's just not ideal.
		ic <- withTSDelta (liftIO . genInodeCache f)
		void $ linkToAnnex k f ic
	writepointer f k = liftIO $ do
		nukeFile f
		writeFile f (formatPointer k)
