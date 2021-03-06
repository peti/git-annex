{- git-annex benchmark infrastructure
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Benchmark where

import Common
import Types.Benchmark
import Types.Command
import CmdLine.Action
import CmdLine
import qualified Annex
import qualified Annex.Branch

import qualified Options.Applicative as O

{- Given a list of all git-annex Commands, and the user's input,
 - generates an IO action to benchmark that runs the specified
 - commands. -}
mkGenerator :: MkBenchmarkGenerator
mkGenerator cmds userinput = do
	-- Get the git-annex branch updated, to avoid the overhead of doing
	-- so skewing the runtime of the first action that will be
	-- benchmarked.
	Annex.Branch.commit "benchmarking"
	_ <- Annex.Branch.update
	l <- mapM parsesubcommand $ split [";"] userinput
	return $ do
		forM_ l $ \(cmd, seek, st) ->
			-- The cmd is run for benchmarking without startup or
			-- shutdown actions.
			Annex.eval st $ performCommandAction False cmd seek noop
  where
	-- Simplified version of CmdLine.dispatch, without support for fuzzy
	-- matching or out-of-repo commands.
	parsesubcommand ps = do
		(cmd, seek, globalconfig) <- liftIO $ O.handleParseResult $
			parseCmd "git-annex" "benchmarking" ps cmds cmdparser
		-- Make an entirely separate Annex state for each subcommand,
		-- and prepare it to run the cmd.
		st <- liftIO . Annex.new =<< Annex.getState Annex.repo
		((), st') <- liftIO $ Annex.run st $
			prepRunCommand cmd globalconfig
		return (cmd, seek, st')
