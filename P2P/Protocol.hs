{- P2P protocol
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module P2P.Protocol where

import qualified Utility.SimpleProtocol as Proto
import Types.Key
import Types.UUID
import Utility.AuthToken
import Utility.Applicative
import Utility.PartialPrelude
import Utility.Metered
import Git.FilePath
import Annex.ChangedRefs (ChangedRefs)

import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Catch
import System.FilePath
import System.Exit (ExitCode(..))
import System.IO
import qualified Data.ByteString.Lazy as L
import Data.Char
import Control.Applicative
import Prelude

newtype Offset = Offset Integer
	deriving (Show)

newtype Len = Len Integer
	deriving (Show)

-- | Service as used by the connect message is gitremote-helpers(1)
data Service = UploadPack | ReceivePack
	deriving (Show)

-- | Messages in the protocol. The peer that makes the connection
-- always initiates requests, and the other peer makes responses to them.
data Message
	= AUTH UUID AuthToken -- uuid of the peer that is authenticating
	| AUTH_SUCCESS UUID -- uuid of the remote peer
	| AUTH_FAILURE
	| CONNECT Service
	| CONNECTDONE ExitCode
	| NOTIFYCHANGE
	| CHANGED ChangedRefs
	| CHECKPRESENT Key
	| LOCKCONTENT Key
	| UNLOCKCONTENT
	| REMOVE Key
	| GET Offset AssociatedFile Key
	| PUT AssociatedFile Key
	| PUT_FROM Offset
	| ALREADY_HAVE
	| CONFIGLIST
	| CONFIGLINE String
	| CONFIGLISTDONE
	| SUCCESS
	| FAILURE
	| DATA Len -- followed by bytes of data
	| ERROR String
	deriving (Show)

instance Proto.Sendable Message where
	formatMessage (AUTH uuid authtoken) = ["AUTH", Proto.serialize uuid, Proto.serialize authtoken]
	formatMessage (AUTH_SUCCESS uuid) = ["AUTH-SUCCESS",  Proto.serialize uuid]
	formatMessage AUTH_FAILURE = ["AUTH-FAILURE"]
	formatMessage (CONNECT service) = ["CONNECT", Proto.serialize service]
	formatMessage (CONNECTDONE exitcode) = ["CONNECTDONE", Proto.serialize exitcode]
	formatMessage NOTIFYCHANGE = ["NOTIFYCHANGE"]
	formatMessage (CHANGED refs) = ["CHANGED", Proto.serialize refs]
	formatMessage (CHECKPRESENT key) = ["CHECKPRESENT", Proto.serialize key]
	formatMessage (LOCKCONTENT key) = ["LOCKCONTENT", Proto.serialize key]
	formatMessage UNLOCKCONTENT = ["UNLOCKCONTENT"]
	formatMessage (REMOVE key) = ["REMOVE", Proto.serialize key]
	formatMessage (GET offset af key) = ["GET", Proto.serialize offset, Proto.serialize af, Proto.serialize key]
	formatMessage (PUT af key) = ["PUT", Proto.serialize af, Proto.serialize key]
	formatMessage (PUT_FROM offset) = ["PUT-FROM", Proto.serialize offset]
	formatMessage ALREADY_HAVE = ["ALREADY-HAVE"]
	formatMessage CONFIGLIST = ["CONFIGLIST"]
	formatMessage (CONFIGLINE l) = ["CONFIGLINE", Proto.serialize l]
	formatMessage CONFIGLISTDONE = ["CONFIGLISTDONE"]
	formatMessage SUCCESS = ["SUCCESS"]
	formatMessage FAILURE = ["FAILURE"]
	formatMessage (DATA len) = ["DATA", Proto.serialize len]
	formatMessage (ERROR err) = ["ERROR", Proto.serialize err]

instance Proto.Receivable Message where
	parseCommand "AUTH" = Proto.parse2 AUTH
	parseCommand "AUTH-SUCCESS" = Proto.parse1 AUTH_SUCCESS
	parseCommand "AUTH-FAILURE" = Proto.parse0 AUTH_FAILURE
	parseCommand "CONNECT" = Proto.parse1 CONNECT
	parseCommand "CONNECTDONE" = Proto.parse1 CONNECTDONE
	parseCommand "NOTIFYCHANGE" = Proto.parse0 NOTIFYCHANGE
	parseCommand "CHANGED" = Proto.parse1 CHANGED
	parseCommand "CHECKPRESENT" = Proto.parse1 CHECKPRESENT
	parseCommand "LOCKCONTENT" = Proto.parse1 LOCKCONTENT
	parseCommand "UNLOCKCONTENT" = Proto.parse0 UNLOCKCONTENT
	parseCommand "REMOVE" = Proto.parse1 REMOVE
	parseCommand "GET" = Proto.parse3 GET
	parseCommand "PUT" = Proto.parse2 PUT
	parseCommand "PUT-FROM" = Proto.parse1 PUT_FROM
	parseCommand "ALREADY-HAVE" = Proto.parse0 ALREADY_HAVE
	parseCommand "CONFIGLIST" = Proto.parse0 CONFIGLIST
	parseCommand "CONFIGLINE" =  Proto.parse1 CONFIGLINE
	parseCommand "CONFIGLISTDONE" = Proto.parse0 CONFIGLISTDONE
	parseCommand "SUCCESS" = Proto.parse0 SUCCESS
	parseCommand "FAILURE" = Proto.parse0 FAILURE
	parseCommand "DATA" = Proto.parse1 DATA
	parseCommand "ERROR" = Proto.parse1 ERROR
	parseCommand _ = Proto.parseFail

instance Proto.Serializable Offset where
	serialize (Offset n) = show n
	deserialize = Offset <$$> readish

instance Proto.Serializable Len where
	serialize (Len n) = show n
	deserialize = Len <$$> readish

instance Proto.Serializable Service where
	serialize UploadPack = "git-upload-pack"
	serialize ReceivePack = "git-receive-pack"
	deserialize "git-upload-pack" = Just UploadPack
	deserialize "git-receive-pack" = Just ReceivePack
	deserialize _ = Nothing

-- | Since AssociatedFile is not the last thing in a protocol line,
-- its serialization cannot contain any whitespace. This is handled
-- by replacing whitespace with '%' (and '%' with '%%')
--
-- When deserializing an AssociatedFile from a peer, it's sanitized,
-- to avoid any unusual characters that might cause problems when it's
-- displayed to the user.
--
-- These mungings are ok, because an AssociatedFile is only ever displayed
-- to the user and does not need to match a file on disk.
instance Proto.Serializable AssociatedFile where
	serialize Nothing = ""
	serialize (Just af) = toInternalGitPath $ concatMap esc af
	  where
		esc '%' = "%%"
		esc c 
			| isSpace c = "%"
			| otherwise = [c]
	
	deserialize s = case fromInternalGitPath $ deesc [] s of
		[] -> Just Nothing
		f
			| isRelative f -> Just (Just f)
			| otherwise -> Nothing
	  where
	  	deesc b [] = reverse b
		deesc b ('%':'%':cs) = deesc ('%':b) cs
		deesc b ('%':cs) = deesc ('_':b) cs
		deesc b (c:cs)
			| isControl c = deesc ('_':b) cs
			| otherwise = deesc (c:b) cs

-- | Free monad for the protocol, combining net communication,
-- and local actions.
data ProtoF c = Net (NetF c) | Local (LocalF c)
	deriving (Functor)

type Proto = Free ProtoF

net :: Net a -> Proto a
net = hoistFree Net

local :: Local a -> Proto a
local = hoistFree Local

data NetF c
	= SendMessage Message c
	| ReceiveMessage (Message -> c)
	| SendBytes Len L.ByteString MeterUpdate c
	-- ^ Sends exactly Len bytes of data. (Any more or less will
	-- confuse the receiver.)
	| ReceiveBytes Len MeterUpdate (L.ByteString -> c)
	-- ^ Lazily reads bytes from peer. Stops once Len are read,
	-- or if connection is lost, and in either case returns the bytes
	-- that were read. This allows resuming interrupted transfers.
	| CheckAuthToken UUID AuthToken (Bool -> c)
	| RelayService Service c
	-- ^ Runs a service, relays its output to the peer, and data
	-- from the peer to it.
	| Relay RelayHandle RelayHandle (ExitCode -> c)
	-- ^ Reads from the first RelayHandle, and sends the data to a
	-- peer, while at the same time accepting input from the peer
	-- which is sent the the second RelayHandle. Continues until 
	-- the peer sends an ExitCode.
	deriving (Functor)

type Net = Free NetF

newtype RelayHandle = RelayHandle Handle

data LocalF c
	= TmpContentSize Key (Len -> c)
	-- ^ Gets size of the temp file where received content may have
	-- been stored. If not present, returns 0.
	| FileSize FilePath (Len -> c)
	-- ^ Gets size of the content of a file. If not present, returns 0.
	| ContentSize Key (Maybe Len -> c)
	-- ^ Gets size of the content of a key, when the full content is
	-- present.
	| ReadContent Key AssociatedFile Offset (L.ByteString -> Proto Bool) (Bool -> c)
	-- ^ Reads the content of a key and sends it to the callback.
	-- Note that the content may change while it's being sent.
	-- If the content is not available, sends L.empty to the callback.
	| StoreContent Key AssociatedFile Offset Len (Proto L.ByteString) (Bool -> c)
	-- ^ Stores content to the key's temp file starting at an offset.
	-- Once the whole content of the key has been stored, moves the
	-- temp file into place as the content of the key, and returns True.
	--
	-- Note: The ByteString may not contain the entire remaining content
	-- of the key. Only once the temp file size == Len has the whole
	-- content been transferred.
	| StoreContentTo FilePath Offset Len (Proto L.ByteString) (Bool -> c)
	-- ^ Stores the content to a temp file starting at an offset.
	-- Once the whole content of the key has been stored, returns True.
	--
	-- Note: The ByteString may not contain the entire remaining content
	-- of the key. Only once the temp file size == Len has the whole
	-- content been transferred.
	| SetPresent Key UUID c
	| CheckContentPresent Key (Bool -> c)
	-- ^ Checks if the whole content of the key is locally present.
	| RemoveContent Key (Bool -> c)
	-- ^ If the content is not present, still succeeds.
	-- May fail if not enough copies to safely drop, etc.
	| TryLockContent Key (Bool -> Proto ()) c
	-- ^ Try to lock the content of a key,  preventing it
	-- from being deleted, while running the provided protocol
	-- action. If unable to lock the content, runs the protocol action
	-- with False.
	| WaitRefChange (ChangedRefs -> c)
	-- ^ Waits for one or more git refs to change and returns them.
	| GetConfigList ([String] -> c)
	-- ^ Gets the git config list in the git config --list format.
	-- May choose to only send some configs.
	deriving (Functor)

type Local = Free LocalF

-- Generate sendMessage etc functions for all free monad constructors.
$(makeFree ''NetF)
$(makeFree ''LocalF)

auth :: UUID -> AuthToken -> Proto (Maybe UUID)
auth myuuid t = do
	net $ sendMessage (AUTH myuuid t)
	r <- net receiveMessage
	case r of
		AUTH_SUCCESS theiruuid -> return $ Just theiruuid
		AUTH_FAILURE -> return Nothing
		_ -> do
			net $ sendMessage (ERROR "auth failed")
			return Nothing

checkPresent :: Key -> Proto Bool
checkPresent key = do
	net $ sendMessage (CHECKPRESENT key)
	checkSuccess

{- Locks content to prevent it from being dropped, while running an action.
 -
 - Note that this only guarantees that the content is locked as long as the
 - connection to the peer remains up. If the connection is unexpectededly
 - dropped, the peer will then unlock the content.
 -}
lockContentWhile 
	:: MonadMask m 
	=> (forall r. r -> Proto r -> m r)
	-> Key
	-> (Bool -> m a)
	-> m a
lockContentWhile runproto key a = bracket setup cleanup a
  where
	setup = runproto False $ do
		net $ sendMessage (LOCKCONTENT key)
		checkSuccess
	cleanup True = runproto () $ net $ sendMessage UNLOCKCONTENT
	cleanup False = return ()

remove :: Key -> Proto Bool
remove key = do
	net $ sendMessage (REMOVE key)
	checkSuccess

get :: FilePath -> Key -> AssociatedFile -> MeterUpdate -> Proto Bool
get dest key af p = receiveContent p sizer storer (\offset -> GET offset af key)
  where
	sizer = fileSize dest
	storer = storeContentTo dest

put :: Key -> AssociatedFile -> MeterUpdate -> Proto Bool
put key af p = do
	net $ sendMessage (PUT af key)
	r <- net receiveMessage
	case r of
		PUT_FROM offset -> sendContent key af offset p
		ALREADY_HAVE -> return True
		_ -> do
			net $ sendMessage (ERROR "expected PUT_FROM")
			return False

data ServerHandler a
	= ServerGot a
	| ServerContinue
	| ServerUnexpected

-- Server loop, getting messages from the client and handling them
serverLoop :: (Message -> Proto (ServerHandler a)) -> Proto (Maybe a)
serverLoop a = do
	cmd <- net receiveMessage
	case cmd of
		-- When the client sends ERROR to the server, the server
		-- gives up, since it's not clear what state the client
		-- is in, and so not possible to recover.
		ERROR _ -> return Nothing
		_ -> do
			v <- a cmd
			case v of
				ServerGot r -> return (Just r)
				ServerContinue -> serverLoop a
				-- If the client sends an unexpected message,
				-- the server will respond with ERROR, and
				-- always continues processing messages.
				--
				-- Since the protocol is not versioned, this
				-- is necessary to handle protocol changes
				-- robustly, since the client can detect when
				-- it's talking to a server that does not
				-- support some new feature, and fall back.
				ServerUnexpected -> do
					net $ sendMessage (ERROR "unexpected command")
					serverLoop a

-- | Serve the protocol, with an unauthenticated peer. Once the peer
-- successfully authenticates, returns their UUID.
serveAuth :: UUID -> Proto (Maybe UUID)
serveAuth myuuid = serverLoop handler
  where
	handler (AUTH theiruuid authtoken) = do
		ok <- net $ checkAuthToken theiruuid authtoken
		if ok
			then do
				net $ sendMessage (AUTH_SUCCESS myuuid)
				return (ServerGot theiruuid)
			else do
				net $ sendMessage AUTH_FAILURE
				return ServerContinue
	handler _ = return ServerUnexpected

-- | Serve the protocol, with a peer that has authenticated.
serveAuthed :: UUID -> Proto ()
serveAuthed myuuid = void $ serverLoop handler
  where
	handler (LOCKCONTENT key) = do
		local $ tryLockContent key $ \locked -> do
			sendSuccess locked
			when locked $ do
				r' <- net receiveMessage
				case r' of
					UNLOCKCONTENT -> return ()
					_ -> net $ sendMessage (ERROR "expected UNLOCKCONTENT")
		return ServerContinue
	handler (CHECKPRESENT key) = do
		sendSuccess =<< local (checkContentPresent key)
		return ServerContinue
	handler (REMOVE key) = do
		sendSuccess =<< local (removeContent key)
		return ServerContinue
	handler (PUT af key) = do
		have <- local $ checkContentPresent key
		if have
			then net $ sendMessage ALREADY_HAVE
			else do
				let sizer = tmpContentSize key
				let storer = storeContent key af
				ok <- receiveContent nullMeterUpdate sizer storer PUT_FROM
				when ok $
					local $ setPresent key myuuid
		return ServerContinue
	handler (GET offset key af) = do
		void $ sendContent af key offset nullMeterUpdate
		-- setPresent not called because the peer may have
		-- requested the data but not permanently stored it.
		return ServerContinue
	handler (CONNECT service) = do
		net $ relayService service
		-- After connecting to git, there may be unconsumed data
		-- from the git processes hanging around (even if they
		-- exited successfully), so stop serving this connection.
		return $ ServerGot ()
	handler NOTIFYCHANGE = do
		refs <- local waitRefChange
		net $ sendMessage (CHANGED refs)
		return ServerContinue
	handler CONFIGLIST = do
		sendConfigList
		return ServerContinue
	handler _ = return ServerUnexpected

sendContent :: Key -> AssociatedFile -> Offset -> MeterUpdate -> Proto Bool
sendContent key af offset@(Offset n) p = go =<< local (contentSize key)
  where
 	go Nothing = sender (Len 0) L.empty
	go (Just (Len totallen)) = do
		let len = totallen - n
		if len <= 0
			then sender (Len 0) L.empty
			else local $ readContent key af offset $
				sender (Len len)
	sender len content = do
		let p' = offsetMeterUpdate p (toBytesProcessed n)
		net $ sendMessage (DATA len)
		net $ sendBytes len content p'
		checkSuccess

receiveContent :: MeterUpdate -> Local Len -> (Offset -> Len -> Proto L.ByteString -> Local Bool) -> (Offset -> Message) -> Proto Bool
receiveContent p sizer storer mkmsg = do
	Len n <- local sizer
	let p' = offsetMeterUpdate p (toBytesProcessed n)
	let offset = Offset n
	net $ sendMessage (mkmsg offset)
	r <- net receiveMessage
	case r of
		DATA len -> do
			ok <- local $ storer offset len
				(net (receiveBytes len p'))
			sendSuccess ok
			return ok
		_ -> do
			net $ sendMessage (ERROR "expected DATA")
			return False

checkSuccess :: Proto Bool
checkSuccess = do
	ack <- net receiveMessage
	case ack of
		SUCCESS -> return True
		FAILURE -> return False
		_ -> do
			net $ sendMessage (ERROR "expected SUCCESS or FAILURE")
			return False

sendSuccess :: Bool -> Proto ()
sendSuccess True = net $ sendMessage SUCCESS
sendSuccess False = net $ sendMessage FAILURE

notifyChange :: Proto (Maybe ChangedRefs)
notifyChange = do
	net $ sendMessage NOTIFYCHANGE
	ack <- net receiveMessage
	case ack of
		CHANGED rs -> return (Just rs)
		_ -> do
			net $ sendMessage (ERROR "expected CHANGED")
			return Nothing

connect :: Service -> Handle -> Handle -> Proto ExitCode
connect service hin hout = do
	net $ sendMessage (CONNECT service)
	net $ relay (RelayHandle hin) (RelayHandle hout)

data RelayData
	= RelayToPeer L.ByteString
	| RelayFromPeer L.ByteString
	| RelayDone ExitCode
	deriving (Show)

relayFromPeer :: Net RelayData
relayFromPeer = do
	r <- receiveMessage
	case r of
		CONNECTDONE exitcode -> return $ RelayDone exitcode
		DATA len -> RelayFromPeer <$> receiveBytes len nullMeterUpdate
		_ -> do
			sendMessage $ ERROR "expected DATA or CONNECTDONE"
			return $ RelayDone $ ExitFailure 1

relayToPeer :: RelayData -> Net ()
relayToPeer (RelayDone exitcode) = sendMessage (CONNECTDONE exitcode)
relayToPeer (RelayToPeer b) = do
	let len = Len $ fromIntegral $ L.length b
	sendMessage (DATA len)
	sendBytes len b nullMeterUpdate
relayToPeer (RelayFromPeer _) = return ()

configList :: Proto [String]
configList = net $ do
	sendMessage CONFIGLIST
	collectlines []
  where
	collectlines ls = do
		r <- receiveMessage
		case r of
			CONFIGLINE l -> collectlines (l:ls)
			CONFIGLISTDONE -> return (reverse ls)
			_ -> do
				sendMessage $ ERROR "expected CONFIGLINE or CONFIGLISTDONE"
				return []

sendConfigList :: Proto ()
sendConfigList = do
	ls <- local getConfigList
	net $ forM_ ls $
		sendMessage . CONFIGLINE
	net $ sendMessage CONFIGLISTDONE
