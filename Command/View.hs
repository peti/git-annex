{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.View where

import Command
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch
import Types.View
import Annex.View
import Logs.View

cmd :: Command
cmd = notBareRepo $
	command "view" SectionMetaData "enter a view branch"
		paramView (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [] = error "Specify metadata to include in view"
start ps = do
	showStart "view" ""
	view <- mkView ps
	go view  =<< currentView
  where
	go view Nothing = next $ perform view
	go view (Just v)
		| v == view = stop
		| otherwise = error "Already in a view. Use the vfilter and vadd commands to further refine this view."

perform :: View -> CommandPerform
perform view = do
	showAction "searching"
	next $ checkoutViewBranch view applyView

paramView :: String
paramView = paramRepeating "FIELD=VALUE"

mkView :: [String] -> Annex View
mkView ps = go =<< inRepo Git.Branch.current
  where
	go Nothing = error "not on any branch!"
	go (Just b) = return $ fst $ refineView (View b []) $
		map parseViewParam $ reverse ps

checkoutViewBranch :: View -> (View -> Annex Git.Branch) -> CommandCleanup
checkoutViewBranch view mkbranch = do
	here <- liftIO getCurrentDirectory

	branch <- mkbranch view
	
	showOutput
	ok <- inRepo $ Git.Command.runBool
		[ Param "checkout"
		, Param (Git.fromRef $ Git.Ref.base branch)
		]
	when ok $ do
		setView view
		{- A git repo can easily have empty directories in it,
		 - and this pollutes the view, so remove them. -}
		top <- fromRepo Git.repoPath
		liftIO $ removeemptydirs top
		unlessM (liftIO $ doesDirectoryExist here) $ do
			showLongNote (cwdmissing top)
	return ok
  where
	removeemptydirs top = mapM_ (tryIO . removeDirectory)
		=<< dirTreeRecursiveSkipping (".git" `isSuffixOf`) top
	cwdmissing top = unlines
		[ "This view does not include the subdirectory you are currently in."
		, "Perhaps you should:  cd " ++ top
		]
