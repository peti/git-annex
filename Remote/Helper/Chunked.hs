{- git-annex chunked remotes
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Chunked (
	ChunkSize,
	ChunkConfig(..),
	describeChunkConfig,
	getChunkConfig,
	storeChunks,
	removeChunks,
	retrieveChunks,
	checkPresentChunks,
) where

import Common.Annex
import Utility.DataUnits
import Types.StoreRetrieve
import Types.Remote
import Types.Key
import Logs.Chunk
import Utility.Metered
import Crypto (EncKey)
import Backend (isStableKey)

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

data ChunkConfig
	= NoChunks
	| UnpaddedChunks ChunkSize
	| LegacyChunks ChunkSize
	deriving (Show)

describeChunkConfig :: ChunkConfig -> String
describeChunkConfig NoChunks = "none"
describeChunkConfig (UnpaddedChunks sz) = describeChunkSize sz ++ "chunks"
describeChunkConfig (LegacyChunks sz) = describeChunkSize sz ++ " chunks (old style)"

describeChunkSize :: ChunkSize -> String
describeChunkSize sz = roughSize storageUnits False (fromIntegral sz)

noChunks :: ChunkConfig -> Bool
noChunks NoChunks = True
noChunks _ = False

getChunkConfig :: RemoteConfig -> ChunkConfig
getChunkConfig m =
	case M.lookup "chunksize" m of
		Nothing -> case M.lookup "chunk" m of
			Nothing -> NoChunks
			Just v -> readsz UnpaddedChunks v "chunk"
		Just v -> readsz LegacyChunks v "chunksize"
  where
	readsz c v f = case readSize dataUnits v of
		Just size
			| size == 0 -> NoChunks
			| size > 0 -> c (fromInteger size)
		_ -> error $ "bad configuration " ++ f ++ "=" ++ v

-- An infinite stream of chunk keys, starting from chunk 1.
newtype ChunkKeyStream = ChunkKeyStream [Key]

chunkKeyStream :: Key -> ChunkSize -> ChunkKeyStream
chunkKeyStream basek chunksize = ChunkKeyStream $ map mk [1..]
  where
	mk chunknum = sizedk { keyChunkNum = Just chunknum }
	sizedk = basek { keyChunkSize = Just (toInteger chunksize) }

nextChunkKeyStream :: ChunkKeyStream -> (Key, ChunkKeyStream)
nextChunkKeyStream (ChunkKeyStream (k:l)) = (k, ChunkKeyStream l)
nextChunkKeyStream (ChunkKeyStream []) = undefined -- stream is infinite!

takeChunkKeyStream :: ChunkCount -> ChunkKeyStream -> [Key]
takeChunkKeyStream n (ChunkKeyStream l) = genericTake n l

-- Number of chunks already consumed from the stream.
numChunks :: ChunkKeyStream -> Integer
numChunks = pred . fromJust . keyChunkNum . fst . nextChunkKeyStream

{- Splits up the key's content into chunks, passing each chunk to
 - the storer action, along with a corresponding chunk key and a
 - progress meter update callback.
 -
 - To support resuming, the checker is used to find the first missing
 - chunk key. Storing starts from that chunk.
 -
 - This buffers each chunk in memory, so can use a lot of memory
 - with a large ChunkSize.
 - More optimal versions of this can be written, that rely
 - on L.toChunks to split the lazy bytestring into chunks (typically
 - smaller than the ChunkSize), and eg, write those chunks to a Handle.
 - But this is the best that can be done with the storer interface that
 - writes a whole L.ByteString at a time.
 -}
storeChunks
	:: UUID
	-> ChunkConfig
	-> EncKey
	-> Key
	-> FilePath
	-> MeterUpdate
	-> Storer
	-> CheckPresent
	-> Annex Bool
storeChunks u chunkconfig encryptor k f p storer checker = 
	case chunkconfig of
		(UnpaddedChunks chunksize) | isStableKey k -> 
			bracketIO open close (go chunksize)
		_ -> storer k (FileContent f) p
  where
	open = tryIO $ openBinaryFile f ReadMode

	close (Right h) = hClose h
	close (Left _) = noop

	go _ (Left e) = do
		warning (show e)
		return False
	go chunksize (Right h) = do
		let chunkkeys = chunkKeyStream k chunksize
		(chunkkeys', startpos) <- seekResume h encryptor chunkkeys checker
		b <- liftIO $ L.hGetContents h
		gochunks p startpos chunksize b chunkkeys'

	gochunks :: MeterUpdate -> BytesProcessed -> ChunkSize -> L.ByteString -> ChunkKeyStream -> Annex Bool
	gochunks meterupdate startpos chunksize = loop startpos . splitchunk
	  where
		splitchunk = L.splitAt chunksize
	
		loop bytesprocessed (chunk, bs) chunkkeys
			| L.null chunk && numchunks > 0 = do
				-- Once all chunks are successfully
				-- stored, update the chunk log.
				chunksStored u k (FixedSizeChunks chunksize) numchunks
				return True
			| otherwise = do
				liftIO $ meterupdate' zeroBytesProcessed
				let (chunkkey, chunkkeys') = nextChunkKeyStream chunkkeys
				ifM (storer chunkkey (ByteContent chunk) meterupdate')
					( do
						let bytesprocessed' = addBytesProcessed bytesprocessed (L.length chunk)
						loop bytesprocessed' (splitchunk bs) chunkkeys'
					, return False
					)
		  where
			numchunks = numChunks chunkkeys
			{- The MeterUpdate that is passed to the action
			 - storing a chunk is offset, so that it reflects
			 - the total bytes that have already been stored
			 - in previous chunks. -}
			meterupdate' = offsetMeterUpdate meterupdate bytesprocessed

{- Check if any of the chunk keys are present. If found, seek forward
 - in the Handle, so it will be read starting at the first missing chunk.
 - Returns the ChunkKeyStream truncated to start at the first missing
 - chunk, and the number of bytes skipped due to resuming.
 -
 - As an optimisation, if the file fits into a single chunk, there's no need
 - to check if that chunk is present -- we know it's not, because otherwise
 - the whole file would be present and there would be no reason to try to
 - store it.
 -}
seekResume
	:: Handle
	-> EncKey
	-> ChunkKeyStream
	-> CheckPresent
	-> Annex (ChunkKeyStream, BytesProcessed)
seekResume h encryptor chunkkeys checker = do
	sz <- liftIO (hFileSize h)
	if sz <= fromMaybe 0 (keyChunkSize $ fst $ nextChunkKeyStream chunkkeys)
		then return (chunkkeys, zeroBytesProcessed)
		else check 0 chunkkeys sz
  where
	check pos cks sz
		| pos >= sz = do
			-- All chunks are already stored!
			liftIO $ hSeek h AbsoluteSeek sz
			return (cks, toBytesProcessed sz)
		| otherwise = do
			v <- tryNonAsync (checker (encryptor k))
			case v of
				Right True ->
					check pos' cks' sz
				_ -> do
					when (pos > 0) $
						liftIO $ hSeek h AbsoluteSeek pos
					return (cks, toBytesProcessed pos)
	  where
		(k, cks') = nextChunkKeyStream cks
		pos' = pos + fromMaybe 0 (keyChunkSize k)

{- Removes all chunks of a key from a remote, by calling a remover
 - action on each.
 -
 - The remover action should succeed even if asked to
 - remove a key that is not present on the remote.
 -
 - This action may be called on a chunked key. It will simply remove it.
 -}
removeChunks :: (Key -> Annex Bool) -> UUID -> ChunkConfig -> EncKey -> Key -> Annex Bool
removeChunks remover u chunkconfig encryptor k = do
	ls <- chunkKeys u chunkconfig k
	ok <- allM (remover . encryptor) (concat ls)
	when ok $ do
		let chunksizes = catMaybes $ map (keyChunkSize <=< headMaybe) ls
		forM_ chunksizes $ chunksRemoved u k . FixedSizeChunks . fromIntegral
	return ok

{- Retrieves a key from a remote, using a retriever action.
 -
 - When the remote is chunked, tries each of the options returned by
 - chunkKeys until it finds one where the retriever successfully
 - gets the first chunked key. The content of that key, and any
 - other chunks in the list is fed to the sink.
 -
 - If retrival of one of the subsequent chunks throws an exception,
 - gives up and returns False. Note that partial data may have been
 - written to the sink in this case.
 -
 - Resuming is supported when using chunks. When the destination file
 - already exists, it skips to the next chunked key that would be needed
 - to resume.
 -}
retrieveChunks 
	:: Retriever
	-> UUID
	-> ChunkConfig
	-> EncKey
	-> Key
	-> FilePath
	-> MeterUpdate
	-> (Maybe Handle -> Maybe MeterUpdate -> ContentSource -> Annex Bool)
	-> Annex Bool
retrieveChunks retriever u chunkconfig encryptor basek dest basep sink
	| noChunks chunkconfig =
		-- Optimisation: Try the unchunked key first, to avoid
		-- looking in the git-annex branch for chunk counts
		-- that are likely not there.
		getunchunked `catchNonAsync`
			const (go =<< chunkKeysOnly u basek)
	| otherwise = go =<< chunkKeys u chunkconfig basek
  where
	go ls = do
		currsize <- liftIO $ catchMaybeIO $
			toInteger . fileSize <$> getFileStatus dest
		let ls' = maybe ls (setupResume ls) currsize
		if any null ls'
			then return True -- dest is already complete
			else firstavail currsize ls' `catchNonAsync` giveup

	giveup e = do
		warning (show e)
		return False

	firstavail _ [] = return False
	firstavail currsize ([]:ls) = firstavail currsize ls
	firstavail currsize ((k:ks):ls)
		| k == basek = getunchunked
			`catchNonAsync` (const $ firstavail currsize ls)
		| otherwise = do
			let offset = resumeOffset currsize k
			let p = maybe basep
				(offsetMeterUpdate basep . toBytesProcessed)
				offset
			v <- tryNonAsync $
				retriever (encryptor k) p $ \content ->
					bracketIO (maybe opennew openresume offset) hClose $ \h -> do
						void $ tosink (Just h) p content
						let sz = toBytesProcessed $
							fromMaybe 0 $ keyChunkSize k
						getrest p h sz sz ks
							`catchNonAsync` giveup
			case v of
				Left e
					| null ls -> giveup e
					| otherwise -> firstavail currsize ls
				Right r -> return r

	getrest _ _ _ _ [] = return True
	getrest p h sz bytesprocessed (k:ks) = do
		let p' = offsetMeterUpdate p bytesprocessed
		liftIO $ p' zeroBytesProcessed
		ifM (retriever (encryptor k) p' $ tosink (Just h) p')
			( getrest p h sz (addBytesProcessed bytesprocessed sz) ks
			, giveup "chunk retrieval failed"
			)

	getunchunked = retriever (encryptor basek) basep $ tosink Nothing basep

	opennew = openBinaryFile dest WriteMode

	-- Open the file and seek to the start point in order to resume.
	openresume startpoint = do
		-- ReadWriteMode allows seeking; AppendMode does not.
		h <- openBinaryFile dest ReadWriteMode
		hSeek h AbsoluteSeek startpoint
		return h

	{- Progress meter updating is a bit tricky: If the Retriever
	 - populates a file, it is responsible for updating progress
	 - as the file is being retrieved. 
	 -
	 - However, if the Retriever generates a lazy ByteString,
	 - it is not responsible for updating progress (often it cannot).
	 - Instead, the sink is passed a meter to update as it consumes
	 - the ByteString.
	 -}
	tosink h p content = sink h p' content
	  where
		p'
			| isByteContent content = Just p
			| otherwise = Nothing

{- Can resume when the chunk's offset is at or before the end of
 - the dest file. -}
resumeOffset :: Maybe Integer -> Key -> Maybe Integer
resumeOffset Nothing _ = Nothing
resumeOffset currsize k
	| offset <= currsize = offset
	| otherwise = Nothing
  where
	offset = chunkKeyOffset k

{- Drops chunks that are already present in a file, based on its size.
 - Keeps any non-chunk keys.
 -}
setupResume :: [[Key]] -> Integer -> [[Key]]
setupResume ls currsize = map dropunneeded ls
  where
	dropunneeded [] = []
	dropunneeded l@(k:_) = case keyChunkSize k of
		Just chunksize | chunksize > 0 ->
			genericDrop (currsize `div` chunksize) l
		_ -> l

{- Checks if a key is present in a remote. This requires any one
 - of the lists of options returned by chunkKeys to all check out
 - as being present using the checker action.
 -
 - Throws an exception if the remote is not accessible.
 -}
checkPresentChunks
	:: CheckPresent
	-> UUID
	-> ChunkConfig
	-> EncKey
	-> Key
	-> Annex Bool
checkPresentChunks checker u chunkconfig encryptor basek
	| noChunks chunkconfig = do
		-- Optimisation: Try the unchunked key first, to avoid
		-- looking in the git-annex branch for chunk counts
		-- that are likely not there.
		v <- check basek
		case v of
			Right True -> return True
			Left e -> checklists (Just e) =<< chunkKeysOnly u basek
			_ -> checklists Nothing =<< chunkKeysOnly u basek
	| otherwise = checklists Nothing =<< chunkKeys u chunkconfig basek
  where
	checklists Nothing [] = return False
	checklists (Just deferrederror) [] = throwM deferrederror
	checklists d (l:ls)
		| not (null l) = do
			v <- checkchunks l
			case v of
				Left e -> checklists (Just e) ls
				Right True -> return True
				Right False -> checklists Nothing ls
		| otherwise = checklists d ls
	
	checkchunks :: [Key] -> Annex (Either SomeException Bool)
	checkchunks [] = return (Right True)
	checkchunks (k:ks) = do
		v <- check k
		case v of
			Right True -> checkchunks ks
			Right False -> return $ Right False
			Left e -> return $ Left e

	check = tryNonAsync . checker . encryptor

{- A key can be stored in a remote unchunked, or as a list of chunked keys.
 - This can be the case whether or not the remote is currently configured
 - to use chunking.
 -
 - It's even possible for a remote to have the same key stored multiple
 - times with different chunk sizes!
 -
 - This finds all possible lists of keys that might be on the remote that
 - can be combined to get back the requested key, in order from most to
 - least likely to exist.
 -}
chunkKeys :: UUID -> ChunkConfig -> Key -> Annex [[Key]]
chunkKeys u chunkconfig k = do
	l <- chunkKeysOnly u k
	return $ if noChunks chunkconfig
		then [k] : l
		else l ++ [[k]]

chunkKeysOnly :: UUID -> Key -> Annex [[Key]]
chunkKeysOnly u k = map (toChunkList k) <$> getCurrentChunks u k

toChunkList :: Key -> (ChunkMethod, ChunkCount) -> [Key]
toChunkList k (FixedSizeChunks chunksize, chunkcount) =
	takeChunkKeyStream chunkcount $ chunkKeyStream k chunksize
toChunkList _ (UnknownChunks _, _) = []
