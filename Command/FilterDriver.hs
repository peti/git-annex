{- git-annex command
 -
 - Copyright 2015-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FilterDriver where

import Command
import qualified Annex
import Annex.Content
import Annex.Link
import Annex.FileMatcher
import Annex.Ingest
import Annex.CatFile
import Logs.Location
import qualified Database.Keys
import Git.Protocol.PktLine
import Git.Protocol.LongRunningProcess
import Git.FilePath
import Backend

import qualified Data.ByteString.Lazy as B

cmd :: Command
cmd = noCommit $ noMessages $
	command "filterdriver" SectionPlumbing 
		"git filter driver"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	liftIO $ hSetBinaryMode stdin True
	liftIO $ hSetBinaryMode stdout True
	liftIO (handshake selectrole selectcap stdin stdout) >>= \case
		Left err -> giveup err
		Right (_role, _caps) -> go
  where
	selectrole (Role Client "git-filter") = Right $
		Role Server "git-filter"
	selectrole r = Left $ "unexpected role: " ++ show r

	selectcap (Capability "clean") = True
	selectcap (Capability "smudge") = True
	selectcap _ = False

	go = liftIO getFilterReq >>= \case
		Nothing -> stop
		Just (Left err) -> giveup err
		Just (Right req) -> do
			handleFilterReq req
			go

data FilterReq = SmudgeReq FilePath | CleanReq FilePath
	deriving (Show)

-- Returns Nothing on EOF
getFilterReq :: IO (Maybe (Either String FilterReq))
getFilterReq = go Nothing Nothing
  where
	go mc mp = readPktLine stdin >>= \case
		Nothing -> return Nothing
		Just (Left e) -> return (Just (Left e))
		Just (Right pkt)
			| pkt == flushPkt -> case (mc, mp) of
				(Just c, Just p) -> return $ Just $ Right $ c p
				(Just _, _) -> return $ Just $ 
					Left "unexpected filter request received from git"
				(Nothing, _) -> go Nothing Nothing
			| otherwise -> fromMaybe (go mc mp) $
				parsecommand pkt <|> parsepathname pkt
	  where
		parsecommand pkt = case parseKV "command" id pkt of
			Just "smudge" -> Just $ go (Just SmudgeReq) mp
			Just "clean" -> Just $ go (Just CleanReq) mp
			_ -> Nothing
		parsepathname pkt = case parseKV "pathname" id pkt of
			Just p -> Just $ go mc (Just p)
			Nothing -> Nothing

handleFilterReq :: FilterReq -> Annex ()
-- Clean filter is fed file content on stdin, decides if a file
-- should be stored in the annex, and sends a pointer to its
-- injested content if so. Otherwise, the original content.
--
-- Note that we're not allowed to start sending back content until
-- git has sent everything to us, according to its protocol
-- documentation. So, we necessarily have to buffer everything
-- it sends when it's not annexed content.
-- TODO swap content out to disk to avoid leaking memory
-- when it won't be annexed, and avoid buffering file content in memory
-- when it will be.
handleFilterReq (CleanReq file) = go []
  where
	go l = liftIO (readPktLine stdin) >>= \case
		Nothing -> giveup "git protocol ended unexpectedly"
		Just (Left err) -> giveup err
		Just (Right pkt)
			| pkt == flushPkt -> do
				sendInitialSuccess
				let b = B.fromChunks (reverse l)
				if isJust (parseLinkOrPointer b)
					then sendByteString b
					else ifM (shouldAnnex file)
						( addannex
						, sendByteString b
						)
				sendFinalSuccess
			| otherwise -> go (contentPktLine pkt:l)
	
	addannex = do
		-- Look up the backend that was used
		-- for this file before, so that when
		-- git re-cleans a file its backend does
		-- not change.
		currbackend <- maybe Nothing (maybeLookupBackendVariety . keyVariety)
			<$> catKeyFile file
		-- Ingest the file from disk, not what git sent via the
		-- pipe. They should always be the same, and ingest
		-- needs a file on disk.
		emitPointer
			=<< addannex'
			=<< (\ld -> ingest' currbackend ld Nothing)
			=<< lockDown cfg file
	addannex' (Just k, _) = do
		logStatus k InfoPresent
		return k
	addannex' _ = error "could not add file to the annex"
	cfg = LockDownConfig
		{ lockingFile = False
		, hardlinkFileTmp = False
		}

-- Smudge filter is fed git file content, and if it's a pointer to an
-- available annex object, should send its content. Otherwise,
-- send back the file content as-is.
--
-- Note that we're not allowed to start sending back content until
-- git has sent everything to us, according to its protocol
-- documentation. So, we necessarily have to buffer everything
-- it sends when it's not annexed content.
-- TODO swap large content out to disk to avoid leaking memory
handleFilterReq (SmudgeReq file) = go []
  where
	go l = liftIO (readPktLine stdin) >>= \case
		Nothing -> giveup "git protocol ended unexpectedly"
		Just (Left err) -> giveup err
		Just (Right pkt)
			| pkt == flushPkt -> do
				sendInitialSuccess
				let b = B.fromChunks (reverse l)
				case parseLinkOrPointer b of
					Nothing -> sendByteString b
					Just k -> sendannexcontent b k
				sendFinalSuccess
			| otherwise -> go (contentPktLine pkt:l)

	sendannexcontent b k = do
		Database.Keys.addAssociatedFile k =<< inRepo (toTopFilePath file)
		-- A previous unlocked checkout of the file may have
		-- led to the annex object getting modified;
		-- don't provide such modified content as it
		-- will be confusing. inAnnex will detect such
		-- modifications.
		ifM (inAnnex k)
			( do
				content <- calcRepo (gitAnnexLocation k)
				whenM (annexThin <$> Annex.getGitConfig) $
					warning $ "Not able to honor annex.thin when git is checking out " ++ file ++ " (run git annex fix to re-thin files)"
				sendByteString . fromMaybe b
					=<< liftIO (catchMaybeIO (B.readFile content))
			, sendByteString b
			)

sendInitialSuccess :: Annex ()
sendInitialSuccess = liftIO $ do
	writePktLine stdout $ fromMaybe (error "pkt-line format error") $ 
		formatKV "status" id "success"
	writePktLine stdout flushPkt
	hFlush stdout

sendFinalSuccess :: Annex ()
sendFinalSuccess = liftIO $ do
	writePktLine stdout flushPkt
	hFlush stdout

sendByteString :: B.ByteString -> Annex ()
sendByteString b = liftIO $ do
	mapM_ (writePktLine stdout) (streamPktLine b)
	writePktLine stdout flushPkt
	hFlush stdout

emitPointer :: Key -> Annex ()
emitPointer k = liftIO $ case stringPktLine (formatPointer k) of
	Nothing -> error "pkt-line format error"
	Just pkt -> do
		writePktLine stdout pkt
		writePktLine stdout flushPkt
		hFlush stdout

shouldAnnex :: FilePath -> Annex Bool
shouldAnnex file = do
	matcher <- largeFilesMatcher
	checkFileMatcher matcher file
