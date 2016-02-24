{- git-annex worktree files
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.WorkTree where

import Annex.Common
import Annex.Link
import Annex.CatFile
import Annex.Version

{- Looks up the key corresponding to an annexed file in the work tree,
 - by examining what the file links to.
 -
 - An unlocked file will not have a link on disk, so fall back to
 - looking for a pointer to a key in git.
 -}
lookupFile :: FilePath -> Annex (Maybe Key)
lookupFile file = do
	mkey <- isAnnexLink file
	case mkey of
		Just key -> makeret key
		Nothing -> ifM versionSupportsUnlockedPointers
			( ifM (liftIO $ doesFileExist file)
				( maybe (return Nothing) makeret =<< catKeyFile file
				, return Nothing
				)
			, return Nothing 
			)
  where
	makeret = return . Just

{- Modifies an action to only act on files that are already annexed,
 - and passes the key on to it. -}
whenAnnexed :: (FilePath -> Key -> Annex (Maybe a)) -> FilePath -> Annex (Maybe a)
whenAnnexed a file = ifAnnexed file (a file) (return Nothing)

ifAnnexed :: FilePath -> (Key -> Annex a) -> Annex a -> Annex a
ifAnnexed file yes no = maybe no yes =<< lookupFile file
