{- git-annex command infrastructure
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command (
	module Command,
	module ReExported
) where

import Annex.Common as ReExported
import Annex.WorkTree as ReExported (whenAnnexed, ifAnnexed)
import Types.Command as ReExported
import Types.DeferredParse as ReExported
import CmdLine.Seek as ReExported
import CmdLine.Usage as ReExported
import CmdLine.Action as ReExported
import CmdLine.Option as ReExported
import CmdLine.GlobalSetter as ReExported
import CmdLine.GitAnnex.Options as ReExported
import CmdLine.Batch as ReExported
import Options.Applicative as ReExported hiding (command)
import qualified Git
import Annex.Init
import Config
import Utility.Daemon
import Types.Transfer
import Types.ActionItem
import Types.WorkerPool

{- Generates a normal Command -}
command :: String -> CommandSection -> String -> CmdParamsDesc -> (CmdParamsDesc -> CommandParser) -> Command
command name section desc paramdesc mkparser =
	Command commonChecks False False name paramdesc 
		section desc (mkparser paramdesc) [] Nothing

{- Simple option parser that takes all non-option params as-is. -}
withParams :: (CmdParams -> v) -> CmdParamsDesc -> Parser v
withParams mkseek paramdesc = mkseek <$> cmdParams paramdesc

{- Uses the supplied option parser, which yields a deferred parse,
 - and calls finishParse on the result before passing it to the
 - CommandSeek constructor. -}
(<--<) :: DeferredParseClass a
	=> (a -> CommandSeek) 
	-> (CmdParamsDesc -> Parser a)
	-> CmdParamsDesc
	-> Parser CommandSeek
(<--<) mkseek optparser paramsdesc = 
	(mkseek <=< finishParse) <$> optparser paramsdesc

{- Indicates that a command doesn't need to commit any changes to
 - the git-annex branch. -}
noCommit :: Command -> Command
noCommit c = c { cmdnocommit = True }

{- Indicates that a command should not output the usual messages when
 - starting or stopping processing a file or other item. Unless --json mode
 - is enabled, this also enables quiet output mode, so only things
 - explicitly output by the command are shown and not progress messages
 - etc.
 -}
noMessages :: Command -> Command
noMessages c = c { cmdnomessages = True }

{- Adds a fallback action to a command, that will be run if it's used
 - outside a git repository. -}
noRepo :: (String -> Parser (IO ())) -> Command -> Command
noRepo a c = c { cmdnorepo = Just (a (cmdparamdesc c)) }

{- Adds global options to a command. -}
withGlobalOptions :: [[GlobalOption]] -> Command -> Command
withGlobalOptions os c = c { cmdglobaloptions = cmdglobaloptions c ++ concat os }

{- For start stage to indicate what will be done. -}
starting:: MkActionItem t => String -> t -> CommandPerform -> CommandStart
starting msg t a = next (StartMessage msg (mkActionItem t), a)

{- Use when noMessages was used but the command is going to output
 - usual messages after all. -}
startingUsualMessages :: MkActionItem t => String -> t -> CommandPerform -> CommandStart
startingUsualMessages msg t a = next (StartUsualMessages msg (mkActionItem t), a)

{- When no message should be displayed at start/end, but messages can still 
 - be displayed when using eg includeCommandAction. -}
startingNoMessage :: MkActionItem t => t -> CommandPerform -> CommandStart
startingNoMessage t a = next (StartNoMessage (mkActionItem t), a)

{- For commands that do not display usual start or end messages, 
 - but have some other custom output. -}
startingCustomOutput :: MkActionItem t => t -> CommandPerform -> CommandStart
startingCustomOutput t a = next (CustomOutput (mkActionItem t), a)

{- For perform stage to indicate what step to run next. -}
next :: a -> Annex (Maybe a)
next a = return $ Just a

{- For start and perform stage to indicate nothing needs to be done. -}
stop :: Annex (Maybe a)
stop = return Nothing

{- Stops unless a condition is met. -}
stopUnless :: Annex Bool -> Annex (Maybe a) -> Annex (Maybe a)
stopUnless c a = ifM c ( a , stop )

{- This can be used in the perform stage to run the action that is the bulk
 - of the work to do in that stage. If the action succeeds, then any actions
 - run after it will be scheduled as if they were run in the cleanup stage
 - instead of the perform stage.
 -
 - This is not needed for a perform stage that uses `next` to run the
 - cleanup stage action. But sometimes a larger action is being built up
 - and it's not practical to separate out the cleanup stage part from the
 - rest of the action.
 -}
performJob :: Observable a => Annex a -> Annex a
performJob a = do
	r <- a
	if observeBool r
		then changeStageTo CleanupStage
		else noop
	return r

{- When acting on a failed transfer, stops unless it was in the specified
 - direction. -}
checkFailedTransferDirection :: ActionItem -> Direction -> Annex (Maybe a) -> Annex (Maybe a)
checkFailedTransferDirection ai d = stopUnless (pure check)
  where
	check = case actionItemTransferDirection ai of
		Nothing -> True
		Just d' -> d' == d

commonChecks :: [CommandCheck]
commonChecks = [repoExists]

repoExists :: CommandCheck
repoExists = CommandCheck 0 ensureInitialized

notDirect :: Command -> Command
notDirect = addCheck $ whenM isDirect $
	giveup "You cannot run this command in a direct mode repository."

notBareRepo :: Command -> Command
notBareRepo = addCheck $ whenM (fromRepo Git.repoIsLocalBare) $
	giveup "You cannot run this command in a bare repository."

noDaemonRunning :: Command -> Command
noDaemonRunning = addCheck $ whenM (isJust <$> daemonpid) $
	giveup "You cannot run this command while git-annex watch or git-annex assistant is running."
  where
	daemonpid = liftIO . checkDaemon =<< fromRepo gitAnnexPidFile

dontCheck :: CommandCheck -> Command -> Command
dontCheck check cmd = mutateCheck cmd $ \c -> filter (/= check) c

addCheck :: Annex () -> Command -> Command
addCheck check cmd = mutateCheck cmd $ \c ->
	CommandCheck (length c + 100) check : c

mutateCheck :: Command -> ([CommandCheck] -> [CommandCheck]) -> Command
mutateCheck cmd@(Command { cmdcheck = c }) a = cmd { cmdcheck = a c }
