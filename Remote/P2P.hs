{- git remotes using the git-annex P2P protocol
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.P2P (
	remote,
	chainGen,
	openConnection,
) where

import Annex.Common
import qualified Annex
import qualified P2P.Protocol as P2P
import P2P.Address
import P2P.Annex
import P2P.IO
import P2P.Auth
import Types.Remote
import Types.GitConfig
import qualified Git
import Annex.UUID
import Config
import Config.Cost
import Remote.Helper.Git
import Messages.Progress
import Utility.Metered
import Utility.AuthToken
import Types.NumCopies

import Control.Concurrent
import Control.Concurrent.STM

remote :: RemoteType
remote = RemoteType {
	typename = "p2p",
	-- Remote.Git takes care of enumerating P2P remotes,
	-- and will call chainGen on them.
	enumerate = const (return []),
	generate = \_ _ _ _ -> return Nothing,
	setup = error "P2P remotes are set up using git-annex p2p"
}

chainGen :: P2PAddress -> Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
chainGen addr r u c gc = do
	connpool <- mkConnectionPool
	cst <- remoteCost gc expensiveRemoteCost
	let this = Remote 
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = store u addr connpool
		, retrieveKeyFile = retrieve u addr connpool
		, retrieveKeyFileCheap = \_ _ _ -> return False
		, removeKey = remove u addr connpool
		, lockContent = Just (lock u addr connpool)
		, checkPresent = checkpresent u addr connpool
		, checkPresentCheap = False
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, localpath = Nothing
		, repo = r
		, gitconfig = gc { remoteGitConfig = Just $ extractGitConfig r }
		, readonly = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = gitRepoInfo this
		, claimUrl = Nothing
		, checkUrl = Nothing
	}
	return (Just this)

store :: UUID -> P2PAddress -> ConnectionPool -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store u addr connpool k af p = 
	metered (Just p) k $ \p' -> fromMaybe False
		<$> runProto u addr connpool (P2P.put k af p')

retrieve :: UUID -> P2PAddress -> ConnectionPool -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
retrieve u addr connpool k af dest p = unVerified $ 
	metered (Just p) k $ \p' -> fromMaybe False 
		<$> runProto u addr connpool (P2P.get dest k af p')

remove :: UUID -> P2PAddress -> ConnectionPool -> Key -> Annex Bool
remove u addr connpool k = fromMaybe False
	<$> runProto u addr connpool (P2P.remove k)

checkpresent :: UUID -> P2PAddress -> ConnectionPool -> Key -> Annex Bool
checkpresent u addr connpool k = maybe unavail return
	=<< runProto u addr connpool (P2P.checkPresent k)
  where
	unavail = giveup "can't connect to peer"

lock :: UUID -> P2PAddress -> ConnectionPool -> Key -> (VerifiedCopy -> Annex r) -> Annex r
lock u addr connpool k callback =
	withConnection u addr connpool $ \conn -> do
		connv <- liftIO $ newMVar conn
		let runproto d p = do
			c <- liftIO $ takeMVar connv
			(c', mr) <- runProto' p c
			liftIO $ putMVar connv c'
			return (fromMaybe d mr)
		r <- P2P.lockContentWhile runproto k go
		conn' <- liftIO $ takeMVar connv
		return (conn', r)
  where
	go False = giveup "can't lock content"
	go True = withVerifiedCopy LockedCopy u (return True) callback

-- | A connection to the peer.
data Connection 
	= OpenConnection P2PConnection
	| ClosedConnection

type ConnectionPool = TVar [Connection]

mkConnectionPool :: Annex ConnectionPool
mkConnectionPool = liftIO $ newTVarIO []

-- Runs the Proto action.
runProto :: UUID -> P2PAddress -> ConnectionPool -> P2P.Proto a -> Annex (Maybe a)
runProto u addr connpool a = withConnection u addr connpool (runProto' a)

runProto' :: P2P.Proto a -> Connection -> Annex (Connection, Maybe a)
runProto' _ ClosedConnection = return (ClosedConnection, Nothing)
runProto' a (OpenConnection conn) = do
	v <- runFullProto Client conn a
	-- When runFullProto fails, the connection is no longer usable,
	-- so close it.
	case v of
		Left e -> do
			warning $ "Lost connection to peer (" ++ e ++ ")"
			liftIO $ closeConnection conn
			return (ClosedConnection, Nothing)
		Right r -> return (OpenConnection conn, Just r)

-- Uses an open connection if one is available in the ConnectionPool;
-- otherwise opens a new connection.
--
-- Once the action is done, the connection is added back to the
-- ConnectionPool, unless it's no longer open.
withConnection :: UUID -> P2PAddress -> ConnectionPool -> (Connection -> Annex (Connection, a)) -> Annex a
withConnection u addr connpool a = bracketOnError get cache go
  where
	get = do
		mc <- liftIO $ atomically $ do
			l <- readTVar connpool
			case l of
				[] -> do
					writeTVar connpool []
					return Nothing
				(c:cs) -> do
					writeTVar connpool cs
					return (Just c)
		maybe (openConnection u addr) return mc
	
	cache ClosedConnection = return ()
	cache conn = liftIO $ atomically $ modifyTVar' connpool (conn:)

	go conn = do
		(conn', r) <- a conn
		cache conn'
		return r

openConnection :: UUID -> P2PAddress -> Annex Connection
openConnection u addr = do
	g <- Annex.gitRepo
	v <- liftIO $ tryNonAsync $ connectPeer g addr
	case v of
		Right conn -> do
			myuuid <- getUUID
			authtoken <- fromMaybe nullAuthToken
				<$> loadP2PRemoteAuthToken addr
			res <- liftIO $ runNetProto conn $
				P2P.auth myuuid authtoken
			case res of
				Right (Just theiruuid)
					| u == theiruuid -> return (OpenConnection conn)
					| otherwise -> do
						liftIO $ closeConnection conn
						warning "Remote peer uuid seems to have changed."
						return ClosedConnection
				Right Nothing -> do
					warning "Unable to authenticate with peer."
					liftIO $ closeConnection conn
					return ClosedConnection
				Left e -> do
					warning $ "Problem communicating with peer. (" ++ e ++ ")"
					liftIO $ closeConnection conn
					return ClosedConnection
		Left e -> do
			warning $ "Unable to connect to peer. (" ++ show e ++ ")"
			return ClosedConnection
