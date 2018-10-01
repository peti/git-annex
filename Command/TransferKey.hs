{- git-annex plumbing command (for use by old assistant, and users)
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TransferKey where

import Command
import Annex.Content
import Logs.Location
import Annex.Transfer
import qualified Remote
import Types.Remote

cmd :: Command
cmd = noCommit $
	command "transferkey" SectionPlumbing
		"transfers a key from or to a remote"
		paramKey (seek <--< optParser)

data TransferKeyOptions = TransferKeyOptions
	{ keyOptions :: CmdParams 
	, fromToOptions :: FromToOptions [DeferredParse Remote]
	, fileOption :: AssociatedFile
	}

optParser :: CmdParamsDesc -> Parser TransferKeyOptions
optParser desc  = TransferKeyOptions
	<$> cmdParams desc
	<*> parseFromToOptions
	<*> (AssociatedFile <$> optional (strOption
		( long "file" <> metavar paramFile
		<> help "the associated file"
		)))

instance DeferredParseClass TransferKeyOptions where
	finishParse v = TransferKeyOptions
		<$> pure (keyOptions v)
		<*> finishParse (fromToOptions v)
		<*> pure (fileOption v)

seek :: TransferKeyOptions -> CommandSeek
seek o = withKeys go (keyOptions o)
  where
	go k = case fromToOptions o of
		To dest -> commandActions =<<
			(map (toStart k (fileOption o)) <$> mapM getParsed dest)
		From src -> commandActions =<<
			(map (fromStart k (fileOption o)) <$> mapM getParsed src)

toStart :: Key -> AssociatedFile -> Remote -> CommandStart
toStart key file remote = next $ perform Upload file $
	upload (uuid remote) key file stdRetry $ \p -> do
		ok <- Remote.storeKey remote key file p
		when ok $
			Remote.logStatus remote key InfoPresent
		return ok

fromStart :: Key -> AssociatedFile -> Remote -> CommandStart
fromStart key file remote = next $ perform Upload file $
	download (uuid remote) key file stdRetry $ \p ->
		getViaTmp (retrievalSecurityPolicy remote) (RemoteVerify remote) key $ 
			\t -> Remote.retrieveKeyFile remote key file t p

perform :: Direction -> AssociatedFile -> (NotifyWitness -> Annex Bool) -> CommandPerform
perform direction file a = notifyTransfer direction file a >>= liftIO . exitBool
