{- making local repositories (used by webapp mostly)
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.MakeRepo where

import Assistant.WebApp.Common
import Annex.Init
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Command
import qualified Git.Branch
import qualified Annex
import Annex.UUID
import Types.StandardGroups
import Logs.PreferredContent
import qualified Annex.Branch

{- Makes a new git repository. Or, if a git repository already
 - exists, returns False. -}
makeRepo :: FilePath -> Bool -> IO Bool
makeRepo path bare = ifM (probeRepoExists path)
	( return False
	, do
		(transcript, ok) <-
			processTranscript "git" (toCommand params) Nothing
		unless ok $
			error $ "git init failed!\nOutput:\n" ++ transcript
		return True
	)
  where
	baseparams = [Param "init", Param "--quiet"]
	params
		| bare = baseparams ++ [Param "--bare", File path]
		| otherwise = baseparams ++ [File path]

{- Runs an action in the git repository in the specified directory. -}
inDir :: FilePath -> Annex a -> IO a
inDir dir a = do
	state <- Annex.new =<< Git.Config.read =<< Git.Construct.fromPath dir
	Annex.eval state a

{- Creates a new repository, and returns its UUID. -}
initRepo :: Bool -> Bool -> FilePath -> Maybe String -> Maybe StandardGroup -> IO UUID
initRepo True primary_assistant_repo dir desc mgroup = inDir dir $ do
	initRepo' desc mgroup
	{- Initialize the master branch, so things that expect
	 - to have it will work, before any files are added. -}
	unlessM (Git.Config.isBare <$> gitRepo) $
		void $ inRepo $ Git.Branch.commitCommand Git.Branch.AutomaticCommit
			[ Param "--quiet"
			, Param "--allow-empty"
			, Param "-m"
			, Param "created repository"
			]
	{- Automatic gc is disabled, as it can be slow. Instead, gc is done
	 - once a day.
	 -}
	when primary_assistant_repo $ do
		inRepo $ Git.Command.run
			[Param "config", Param "gc.auto", Param "0"]
	getUUID
{- Repo already exists, could be a non-git-annex repo though so
 - still initialize it. -}
initRepo False _ dir desc mgroup = inDir dir $ do
	initRepo' desc mgroup
	getUUID

initRepo' :: Maybe String -> Maybe StandardGroup -> Annex ()
initRepo' desc mgroup = unlessM isInitialized $ do
	initialize desc Nothing
	u <- getUUID
	maybe noop (defaultStandardGroup u) mgroup
	{- Ensure branch gets committed right away so it is
	 - available for merging immediately. -}
	Annex.Branch.commit "update"

{- Checks if a git repo exists at a location. -}
probeRepoExists :: FilePath -> IO Bool
probeRepoExists dir = isJust <$>
	catchDefaultIO Nothing (Git.Construct.checkForRepo dir)
