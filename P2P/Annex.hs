{- P2P protocol, Annex implementation
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module P2P.Annex
	( RunMode(..)
	, P2PConnection(..)
	, runFullProto
	) where

import Annex.Common
import Annex.Content
import Annex.Transfer
import Annex.ChangedRefs
import P2P.Protocol
import P2P.IO
import Logs.Location
import Types.NumCopies
import Utility.Metered
import qualified Git.Config
import Git.Construct (isRemoteUrlConfig)

import Control.Monad.Free

data RunMode
	= Serving UUID (Maybe ChangedRefsHandle)
	| Client

-- Full interpreter for Proto, that can receive and send objects.
runFullProto :: RunMode -> P2PConnection -> Proto a -> Annex (Either String a)
runFullProto runmode conn = go
  where
	go :: RunProto Annex
	go (Pure v) = return (Right v)
	go (Free (Net n)) = runNet conn go n
	go (Free (Local l)) = runLocal runmode go l

runLocal :: RunMode -> RunProto Annex -> LocalF (Proto a) -> Annex (Either String a)
runLocal runmode runner a = case a of
	TmpContentSize k next -> do
		tmp <- fromRepo $ gitAnnexTmpObjectLocation k
		size <- liftIO $ catchDefaultIO 0 $ getFileSize tmp
		runner (next (Len size))
	FileSize f next -> do
		size <- liftIO $ catchDefaultIO 0 $ getFileSize f
		runner (next (Len size))
	ContentSize k next -> do
		let getsize = liftIO . catchMaybeIO . getFileSize
		size <- inAnnex' isJust Nothing getsize k
		runner (next (Len <$> size))
	ReadContent k af o sender next -> do
		v <- tryNonAsync $ prepSendAnnex k
		case v of
			-- The check can detect if the file
			-- changed while it was transferred, but we don't
			-- use it. Instead, the receiving peer must
			-- AlwaysVerify the content it receives.
			Right (Just (f, _check)) -> do
				v' <- tryNonAsync $
					transfer upload k af $
						sinkfile f o sender
				case v' of
					Left e -> return (Left (show e))
					Right (Left e) -> return (Left (show e))
					Right (Right ok) -> runner (next ok)
			-- content not available
 			Right Nothing -> runner (next False)
			Left e -> return (Left (show e))
	StoreContent k af o l getb next -> do
		ok <- flip catchNonAsync (const $ return False) $
			transfer download k af $ \p ->
				getViaTmp AlwaysVerify k $ \tmp ->
					unVerified $ storefile tmp o l getb p
		runner (next ok)
	StoreContentTo dest o l getb next -> do
		ok <- flip catchNonAsync (const $ return False) $
			storefile dest o l getb nullMeterUpdate
		runner (next ok)
	SetPresent k u next -> do
		v <- tryNonAsync $ logChange k u InfoPresent
		case v of
			Left e -> return (Left (show e))
			Right () -> runner next
	CheckContentPresent k next -> do
		v <- tryNonAsync $ inAnnex k
		case v of
			Left e -> return (Left (show e))
			Right result -> runner (next result)
	RemoveContent k next -> do
		v <- tryNonAsync $
			ifM (Annex.Content.inAnnex k)
				( lockContentForRemoval k $ \contentlock -> do
					removeAnnex contentlock
					logStatus k InfoMissing
					return True
				, return True
				)
		case v of
			Left e -> return (Left (show e))
			Right result -> runner (next result)
	TryLockContent k protoaction next -> do
		v <- tryNonAsync $ lockContentShared k $ \verifiedcopy -> 
			case verifiedcopy of
				LockedCopy _ -> runner (protoaction True)
				_ -> runner (protoaction False)
		-- If locking fails, lockContentShared throws an exception.
		-- Let the peer know it failed.
		case v of
			Left _ -> runner $ do
				protoaction False
				next
			Right _ -> runner next
	WaitRefChange next -> case runmode of
		Serving _ (Just h) -> do
			v <- tryNonAsync $ liftIO $ waitChangedRefs h
			case v of
				Left e -> return (Left (show e))
				Right changedrefs -> runner (next changedrefs)
		_ -> return $ Left "change notification not available"
	GetConfigList next -> do
		g <- Annex.gitRepo
		-- A git config might contain sensative data that
		-- should not be exposed to peers. Only the parts of the
		-- git config needed for the map command are sent:
		-- The remote urls, and annex uuids.
		let ls = Git.Config.format $
			filter (\(k,_) -> isRemoteUrlConfig k || )
			M.toList $ Git.config g
		runner (next ls)
  where
	transfer mk k af ta = case runmode of
		-- Update transfer logs when serving.
		Serving theiruuid _ -> 
			mk theiruuid k af noRetry ta noNotification
		-- Transfer logs are updated higher in the stack when
		-- a client.
		Client -> ta nullMeterUpdate
	
	storefile dest (Offset o) (Len l) getb p = do
		let p' = offsetMeterUpdate p (toBytesProcessed o)
		v <- runner getb
		case v of
			Right b -> liftIO $ do
				withBinaryFile dest ReadWriteMode $ \h -> do
					when (o /= 0) $
						hSeek h AbsoluteSeek o
					meteredWrite p' h b
				sz <- getFileSize dest
				return (toInteger sz == l + o)
			Left e -> error e
	
	sinkfile f (Offset o) sender p = bracket setup cleanup go
	  where
		setup = liftIO $ openBinaryFile f ReadMode
		cleanup = liftIO . hClose
		go h = do
			let p' = offsetMeterUpdate p (toBytesProcessed o)
			when (o /= 0) $
				liftIO $ hSeek h AbsoluteSeek o
			b <- liftIO $ hGetContentsMetered h p'
			runner (sender b)
