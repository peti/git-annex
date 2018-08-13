{- Git long-running process protocol, as documented in 
 - git/Documentation/technical/long-running-process-protocol.txt
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE LambdaCase #-}

module Git.Protocol.LongRunningProcess where

import Git.Protocol.PktLine

import Data.List
import Data.Monoid
import Control.Applicative
import System.IO

data ClientServer = Client | Server
	deriving (Show)

clientServerSuffix :: ClientServer -> String
clientServerSuffix Client = "-client"
clientServerSuffix Server = "-server"

data Role = Role ClientServer String
	deriving (Show)

parseRole :: String -> Maybe Role
parseRole s = go Client <|> go Server
  where
	go cs = 
		let suffix = clientServerSuffix cs
		in if suffix `isSuffixOf` s
			then Just $ Role cs $
				take (length s - length suffix) s
			else Nothing

pktRole :: PktLine -> Maybe Role
pktRole = parseRole . pktLineString

rolePkt :: Role -> Maybe PktLine
rolePkt (Role cs t) = stringPktLine $ t <> clientServerSuffix cs

newtype Capability = Capability { fromCapability :: String }
	deriving (Show, Eq)

pktCapability :: PktLine -> Maybe Capability
pktCapability = parseKV "capability" Capability

capabilityPkt :: Capability -> Maybe PktLine
capabilityPkt = formatKV "capability" fromCapability

newtype Version = Version { fromVersion :: String }
	deriving (Show, Eq)

pktVersion :: PktLine -> Maybe Version
pktVersion = parseKV "version" Version

versionPkt :: Version -> Maybe PktLine
versionPkt = formatKV "version" fromVersion

-- | Runs the protocol's initial handshake.
--
-- The Role selection function should convert a Client role into a
-- Server role; git will be the Client and the program using this module
-- the Server.
handshake
	:: (Role -> Either String Role) -- ^ role selection function
	-> (Capability -> Bool) -- ^ capability selection function
	-> Handle -- ^ handle to receive data from git
	-> Handle -- ^ handle to send data to git
	-> IO (Either String (Role, [Capability]))
handshake selectrole selectcapability input output =
	getpkt pktRole $ \role -> checkversion $ 
		case selectrole role of
			Left e -> return (Left e)
			Right myrole -> sendpkt rolePkt myrole $
				sendpkt versionPkt (Version "2") $ do
					hFlush output
					exchangecaps $ \mycaps -> return $ 
						Right (myrole, mycaps)
  where
	protoerr e = return $ Left $ e ++ " from git in protocol handshake"
	
	sendpkt f v cnt = case f v of
		Just pkt -> do
			writePktLine output pkt
			cnt
		Nothing -> return $ Left $ 
			"failed constructing pkt-line packet for: " ++ show v
	
	sendpkts _ [] cnt = do
		writePktLine output flushPkt
		hFlush output
		cnt
	sendpkts f (v:vs) cnt = sendpkt f v $ sendpkts f vs cnt

	getpkt parser cnt = readPktLine input >>= \case
		Nothing -> protoerr "EOF"
		Just (Left e) -> return (Left e)
		Just (Right pkt) -> case parser pkt of
			Nothing -> protoerr $ "unparsable packet: " ++ show pkt
			Just v -> cnt v
	
	getpkts parser cnt = go []
	  where
		go c = getpkt Just $ \pkt ->
			if pkt == flushPkt
				then cnt (reverse c)
				else case parser pkt of
					Nothing -> protoerr $ "unparsable packet" ++ show pkt
					Just v -> go (v:c)
	
	checkversion cnt = getpkts pktVersion $ \versions ->
		if any (== Version "2") versions
			then cnt
			else return $ Left $
				"git is using an unsupported protocol version: " ++ show versions
	
	exchangecaps cnt = getpkts pktCapability $ \caps -> do
		let mycaps = filter selectcapability caps
		sendpkts capabilityPkt mycaps $
			cnt mycaps

formatKV :: String -> (v -> String) -> v -> Maybe PktLine
formatKV k f v = stringPktLine $ k <> "=" <> f v

parseKV :: String -> (String -> v) -> PktLine -> Maybe v
parseKV k mkv = go . pktLineString
  where
	kprefix = k <> "="
	go t
		| kprefix `isPrefixOf` t = Just $ mkv $ 
			drop (length kprefix) t
		| otherwise = Nothing

