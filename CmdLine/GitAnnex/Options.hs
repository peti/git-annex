{- git-annex command-line option parsing
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP #-}

module CmdLine.GitAnnex.Options where

import Options.Applicative
#if ! MIN_VERSION_optparse_applicative(0,14,1)
import Options.Applicative.Builder.Internal
#endif
import Control.Concurrent
import qualified Data.Map as M

import Annex.Common
import qualified Git.Config
import qualified Git.Construct
import Git.Remote
import Git.Types
import Types.Key
import Types.TrustLevel
import Types.NumCopies
import Types.Messages
import Types.Command
import Types.DeferredParse
import Types.DesktopNotify
import Types.Concurrency
import qualified Annex
import qualified Remote
import qualified Limit
import qualified Limit.Wanted
import CmdLine.Option
import CmdLine.Usage
import CmdLine.GlobalSetter
import qualified Backend
import qualified Types.Backend as Backend
import Utility.HumanTime

-- Global options that are accepted by all git-annex sub-commands,
-- although not always used.
gitAnnexGlobalOptions :: [GlobalOption]
gitAnnexGlobalOptions = commonGlobalOptions ++
	[ globalSetter setnumcopies $ option auto
		( long "numcopies" <> short 'N' <> metavar paramNumber
		<> help "override default number of copies"
		<> hidden
		)
	, globalSetter (Remote.forceTrust Trusted) $ strOption
		( long "trust" <> metavar paramRemote
		<> help "override trust setting"
		<> hidden
		<> completeRemotes
		)
	, globalSetter (Remote.forceTrust SemiTrusted) $ strOption
		( long "semitrust" <> metavar paramRemote
		<> help "override trust setting back to default"
		<> hidden
		<> completeRemotes
		)
	, globalSetter (Remote.forceTrust UnTrusted) $ strOption
		( long "untrust" <> metavar paramRemote
		<> help "override trust setting to untrusted"
		<> hidden
		<> completeRemotes
		)
	, globalSetter setgitconfig $ strOption
		( long "config" <> short 'c' <> metavar "NAME=VALUE"
		<> help "override git configuration setting"
		<> hidden
		)
	, globalSetter setuseragent $ strOption
		( long "user-agent" <> metavar paramName
		<> help "override default User-Agent"
		<> hidden
		)
	, globalFlag (Annex.setFlag "trustglacier")
		( long "trust-glacier"
		<> help "Trust Amazon Glacier inventory"
		<> hidden
		)
	, globalFlag (setdesktopnotify mkNotifyFinish)
		( long "notify-finish"
		<> help "show desktop notification after transfer finishes"
		<> hidden
		)
	, globalFlag (setdesktopnotify mkNotifyStart)
		( long "notify-start"
		<> help "show desktop notification after transfer starts"
		<> hidden
		)
	]
  where
	setnumcopies n = Annex.changeState $ \s -> s { Annex.forcenumcopies = Just $ NumCopies n }
	setuseragent v = Annex.changeState $ \s -> s { Annex.useragent = Just v }
	setgitconfig v = Annex.adjustGitRepo $ \r -> Git.Config.store v $ 
		r { gitGlobalOpts = gitGlobalOpts r ++ [Param "-c", Param v] }
	setdesktopnotify v = Annex.changeState $ \s -> s { Annex.desktopnotify = Annex.desktopnotify s <> v }

{- Parser that accepts all non-option params. -}
cmdParams :: CmdParamsDesc -> Parser CmdParams
cmdParams paramdesc = many $ argument str
	( metavar paramdesc
	<> action "file"
	)

parseAutoOption :: Parser Bool
parseAutoOption = switch
	( long "auto" <> short 'a'
	<> help "automatic mode"
	)

parseRemoteOption :: RemoteName -> DeferredParse Remote
parseRemoteOption = DeferredParse 
	. (fromJust <$$> Remote.byNameWithUUID)
	. Just

-- | From or To a remote.
data FromToOptions r = From r | To r

instance DeferredParseClass (FromToOptions [DeferredParse r]) where
	finishParse (From l) = From <$> finishParse l
	finishParse (To l) = To <$> finishParse l

parseFromToOptions :: Parser (FromToOptions [DeferredParse Remote])
parseFromToOptions = 
	(From . map parseRemoteOption <$> some parseFromOption) 
	<|> (To . map parseRemoteOption <$> some parseToOption)

parseFromOption :: Parser RemoteName
parseFromOption = strOption
	( long "from" <> short 'f' <> metavar paramRemote
	<> help "source remote"
	<> completeRemotes
	)

parseToOption :: Parser RemoteName
parseToOption = strOption
	( long "to" <> short 't' <> metavar paramRemote
	<> help "destination remote"
	<> completeRemotes
	)

-- | Like FromToOptions, but with a special --to=here
type FromToHereOptions r = Either ToHere (FromToOptions r)

data ToHere = ToHere

parseFromToHereOptions :: Parser (FromToHereOptions [DeferredParse Remote])
parseFromToHereOptions = parsefrom <|> parseto
  where
	parsefrom = Right . From . map parseRemoteOption <$> some parseFromOption
	parseto = herespecialcase <$> some parseToOption
	  where
		herespecialcase l
			| any ishere l && all ishere l = Left ToHere
			| any ishere l = fail "Cannot mix --to=here with --to=remote"
			| otherwise = Right $ To $ map parseRemoteOption l
		ishere "here" = True
		ishere "." = True
		ishere _ = False

instance DeferredParseClass (FromToHereOptions [DeferredParse r]) where
	finishParse = either (pure . Left) (Right <$$> finishParse)

-- Options for acting on keys, rather than work tree files.
data KeyOptions
	= WantAllKeys
	| WantUnusedKeys
	| WantFailedTransfers
	| WantSpecificKey Key
	| WantIncompleteKeys
	| WantBranchKeys [Branch]

parseKeyOptions :: Parser KeyOptions
parseKeyOptions = parseAllOption
	<|> WantBranchKeys <$> some (option (str >>= pure . Ref)
		( long "branch" <> metavar paramRef
		<> help "operate on files in the specified branch or treeish"
		))
	<|> flag' WantUnusedKeys
		( long "unused" <> short 'U'
		<> help "operate on files found by last run of git-annex unused"
		)
	<|> (WantSpecificKey <$> option (str >>= parseKey)
		( long "key" <> metavar paramKey
		<> help "operate on specified key"
		))

parseFailedTransfersOption :: Parser KeyOptions
parseFailedTransfersOption = flag' WantFailedTransfers
	( long "failed"
	<> help "operate on files that recently failed to be transferred"
	)

parseIncompleteOption :: Parser KeyOptions
parseIncompleteOption = flag' WantIncompleteKeys
	( long "incomplete"
	<> help "resume previous downloads"
	)

parseAllOption :: Parser KeyOptions
parseAllOption = flag' WantAllKeys
	( long "all" <> short 'A'
	<> help "operate on all versions of all files"
	)

parseKey :: Monad m => String -> m Key
parseKey = maybe (fail "invalid key") return . file2key

-- Options to match properties of annexed files.
annexedMatchingOptions :: [GlobalOption]
annexedMatchingOptions = concat
	[ nonWorkTreeMatchingOptions'
	, fileMatchingOptions'
	, combiningOptions
	, timeLimitOption
	]

-- Matching options that don't need to examine work tree files.
nonWorkTreeMatchingOptions :: [GlobalOption]
nonWorkTreeMatchingOptions = nonWorkTreeMatchingOptions' ++ combiningOptions

nonWorkTreeMatchingOptions' :: [GlobalOption]
nonWorkTreeMatchingOptions' = 
	[ globalSetter Limit.addIn $ strOption
		( long "in" <> short 'i' <> metavar paramRemote
		<> help "match files present in a remote"
		<> hidden
		<> completeRemotes
		)
	, globalSetter Limit.addCopies $ strOption
		( long "copies" <> short 'C' <> metavar paramRemote
		<> help "skip files with fewer copies"
		<> hidden
		)
	, globalSetter (Limit.addLackingCopies False) $ strOption
		( long "lackingcopies" <> metavar paramNumber
		<> help "match files that need more copies"
		<> hidden
		)
	, globalSetter (Limit.addLackingCopies True) $ strOption
		( long "approxlackingcopies" <> metavar paramNumber
		<> help "match files that need more copies (faster)"
		<> hidden
		)
	, globalSetter Limit.addInBackend $ strOption
		( long "inbackend" <> short 'B' <> metavar paramName
		<> help "match files using a key-value backend"
		<> hidden
		<> completeBackends
		)
	, globalFlag Limit.addSecureHash
		( long "securehash"
		<> help "match files using a cryptographically secure hash"
		<> hidden
		)
	, globalSetter Limit.addInAllGroup $ strOption
		( long "inallgroup" <> metavar paramGroup
		<> help "match files present in all remotes in a group"
		<> hidden
		)
	, globalSetter Limit.addMetaData $ strOption
		( long "metadata" <> metavar "FIELD=VALUE"
		<> help "match files with attached metadata"
		<> hidden
		)
	, globalFlag Limit.Wanted.addWantGet
		( long "want-get"
		<> help "match files the repository wants to get"
		<> hidden
		)
	, globalFlag Limit.Wanted.addWantDrop
		( long "want-drop"
		<> help "match files the repository wants to drop"
		<> hidden
		)
	, globalSetter Limit.addAccessedWithin $ option (str >>= parseDuration)
		( long "accessedwithin"
		<> metavar paramTime
		<> help "match files accessed within a time interval"
		<> hidden
		)
	]

-- Options to match files which may not yet be annexed.
fileMatchingOptions :: [GlobalOption]
fileMatchingOptions = fileMatchingOptions' ++ combiningOptions

fileMatchingOptions' :: [GlobalOption]
fileMatchingOptions' =
	[ globalSetter Limit.addExclude $ strOption
		( long "exclude" <> short 'x' <> metavar paramGlob
		<> help "skip files matching the glob pattern"
		<> hidden
		)
	, globalSetter Limit.addInclude $ strOption
		( long "include" <> short 'I' <> metavar paramGlob
		<> help "limit to files matching the glob pattern"
		<> hidden
		)
	, globalSetter Limit.addLargerThan $ strOption
		( long "largerthan" <> metavar paramSize
		<> help "match files larger than a size"
		<> hidden
		)
	, globalSetter Limit.addSmallerThan $ strOption
		( long "smallerthan" <> metavar paramSize
		<> help "match files smaller than a size"
		<> hidden
		)
	]

combiningOptions :: [GlobalOption]
combiningOptions = 
	[ longopt "not" "negate next option"
	, longopt "and" "both previous and next option must match"
	, longopt "or" "either previous or next option must match"
	, shortopt '(' "open group of options"
	, shortopt ')' "close group of options"
	]
  where
	longopt o h = globalFlag (Limit.addToken o) ( long o <> help h <> hidden )
	shortopt o h = globalFlag (Limit.addToken [o]) ( short o <> help h <> hidden )

jsonOptions :: [GlobalOption]
jsonOptions = 
	[ globalFlag (Annex.setOutput (JSONOutput stdjsonoptions))
		( long "json" <> short 'j'
		<> help "enable JSON output"
		<> hidden
		)
	, globalFlag (Annex.setOutput (JSONOutput jsonerrormessagesoptions))
		( long "json-error-messages"
		<> help "include error messages in JSON"
		<> hidden
		)
	]
  where
	stdjsonoptions = JSONOptions
		{ jsonProgress = False
		, jsonErrorMessages = False
		}
	jsonerrormessagesoptions = stdjsonoptions { jsonErrorMessages = True }

jsonProgressOption :: [GlobalOption]
jsonProgressOption = 
	[ globalFlag (Annex.setOutput (JSONOutput jsonoptions))
		( long "json-progress"
		<> help "include progress in JSON output"
		<> hidden
		)
	]
  where
	jsonoptions = JSONOptions
		{ jsonProgress = True
		, jsonErrorMessages = False
		}

-- Note that a command that adds this option should wrap its seek
-- action in `allowConcurrentOutput`.
jobsOption :: [GlobalOption]
jobsOption = 
	[ globalSetter set $ 
		option auto
			( long "jobs" <> short 'J' <> metavar paramNumber
			<> help "enable concurrent jobs"
			<> hidden
			)
	]
  where
	set n = do
		Annex.changeState $ \s -> s { Annex.concurrency = Concurrent n }
		c <- liftIO getNumCapabilities
		when (n > c) $
			liftIO $ setNumCapabilities n

timeLimitOption :: [GlobalOption]
timeLimitOption = 
	[ globalSetter Limit.addTimeLimit $ option (str >>= parseDuration)
		( long "time-limit" <> short 'T' <> metavar paramTime
		<> help "stop after the specified amount of time"
		<> hidden
		)
	]

data DaemonOptions = DaemonOptions
	{ foregroundDaemonOption :: Bool
	, stopDaemonOption :: Bool
	}

parseDaemonOptions :: Parser DaemonOptions
parseDaemonOptions = DaemonOptions
	<$> switch
		( long "foreground"
		<> help "do not daemonize"
		)
	<*> switch
		( long "stop"
		<> help "stop daemon"
		)
completeRemotes :: HasCompleter f => Mod f a
completeRemotes = completer $ mkCompleter $ \input -> do
	r <- maybe (pure Nothing) (Just <$$> Git.Config.read)
		=<< Git.Construct.fromCwd
	return $ filter (input `isPrefixOf`) $
		map remoteKeyToRemoteName $
			filter isRemoteKey $
				maybe [] (M.keys . config) r
		
completeBackends :: HasCompleter f => Mod f a
completeBackends = completeWith $
	map (formatKeyVariety . Backend.backendVariety) Backend.list
