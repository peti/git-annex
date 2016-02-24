{- git-annex repository fixups
 -
 - Copyright 2013, 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Fixup where

import Git.Types
import Git.Config
import Types.GitConfig
import qualified Git.BuildVersion
import Utility.Path
import Utility.SafeCommand
import Utility.Directory
import Utility.PosixFiles
import Utility.Exception

import System.IO
import System.FilePath
import System.Directory
import Data.List
import Control.Monad
import Control.Monad.IfElse
import qualified Data.Map as M

fixupRepo :: Repo -> GitConfig -> IO Repo
fixupRepo r c = do
	let r' = disableWildcardExpansion r
	fixupSubmodule r' c

{- Disable git's built-in wildcard expansion, which is not wanted
 - when using it as plumbing by git-annex. -}
disableWildcardExpansion :: Repo -> Repo
disableWildcardExpansion r
	| Git.BuildVersion.older "1.8.1" = r
	| otherwise = r
		{ gitGlobalOpts = gitGlobalOpts r ++ [Param "--literal-pathspecs"] }

{- Submodules have their gitdir containing ".git/modules/", and
 - have core.worktree set, and also have a .git file in the top
 - of the repo. 
 -
 - We need to unset core.worktree, and change the .git file into a
 - symlink to the git directory. This way, annex symlinks will be
 - of the usual .git/annex/object form, and will consistently work
 - whether a repo is used as a submodule or not, and wheverever the
 - submodule is mounted.
 -
 - When the filesystem doesn't support symlinks, we cannot make .git
 - into a symlink. But we don't need too, since symlinks won't be followed
 - anyway. In this case, we merely adjust the Repo so that
 - symlinks to objects that get checked in will be in the right form.
 -}
fixupSubmodule :: Repo -> GitConfig -> IO Repo
fixupSubmodule r@(Repo { location = l@(Local { worktree = Just w, gitdir = d }) }) c
	| needsSubmoduleFixup r = do
		when (coreSymlinks c) $
			replacedotgit
				`catchNonAsync` \_e -> hPutStrLn stderr
					"warning: unable to convert submodule to form that will work with git-annex"
		return $ r
			{ location = if coreSymlinks c
				then l { gitdir = dotgit }
				else l
			, config = M.delete "core.worktree" (config r)
			}
  where
	dotgit = w </> ".git"
	replacedotgit = whenM (doesFileExist dotgit) $ do
		linktarget <- relPathDirToFile w d
		nukeFile dotgit
		createSymbolicLink linktarget dotgit
		maybe (error "unset core.worktree failed") (\_ -> return ())
			=<< Git.Config.unset "core.worktree" r
fixupSubmodule r _ = return r

needsSubmoduleFixup :: Repo -> Bool
needsSubmoduleFixup (Repo { location = (Local { worktree = Just _, gitdir = d }) }) =
	(".git" </> "modules") `isInfixOf` d
needsSubmoduleFixup _ = False
