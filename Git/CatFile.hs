{- git cat-file interface
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Git.CatFile (
	CatFileHandle,
	catFileStart,
	catFileStart',
	catFileStop,
	catFile,
	catFileDetails,
	catTree,
	catCommit,
	catObject,
	catObjectDetails,
	catObjectMetaData,
	catObjectStream,
	catObjectStream',
) where

import System.IO
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.Map.Strict as M
import Data.String
import Data.Char
import Numeric
import System.Posix.Types
import Text.Read
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO)

import Common
import Git
import Git.Sha
import qualified Git.Ref
import Git.Command
import Git.Types
import Git.FilePath
import Git.HashObject
import qualified Git.LsTree as LsTree
import qualified Utility.CoProcess as CoProcess
import Utility.Tuple
import Utility.TList

data CatFileHandle = CatFileHandle 
	{ catFileProcess :: CoProcess.CoProcessHandle
	, checkFileProcess :: CoProcess.CoProcessHandle
	, gitRepo :: Repo
	}

catFileStart :: Repo -> IO CatFileHandle
catFileStart = catFileStart' True

catFileStart' :: Bool -> Repo -> IO CatFileHandle
catFileStart' restartable repo = CatFileHandle
	<$> startp "--batch"
	<*> startp ("--batch-check=" ++ batchFormat)
	<*> pure repo
  where
	startp p = gitCoProcessStart restartable
		[ Param "cat-file"
		, Param p
		] repo

batchFormat :: String
batchFormat = "%(objectname) %(objecttype) %(objectsize)"

catFileStop :: CatFileHandle -> IO ()
catFileStop h = do
	CoProcess.stop (catFileProcess h)
	CoProcess.stop (checkFileProcess h)

{- Reads a file from a specified branch. -}
catFile :: CatFileHandle -> Branch -> RawFilePath -> IO L.ByteString
catFile h branch file = catObject h $ Ref $
	fromRef' branch <> ":" <> toInternalGitPath file

catFileDetails :: CatFileHandle -> Branch -> RawFilePath -> IO (Maybe (L.ByteString, Sha, ObjectType))
catFileDetails h branch file = catObjectDetails h $ Ref $
	fromRef' branch <> ":" <> toInternalGitPath file

{- Uses a running git cat-file read the content of an object.
 - Objects that do not exist will have "" returned. -}
catObject :: CatFileHandle -> Ref -> IO L.ByteString
catObject h object = maybe L.empty fst3 <$> catObjectDetails h object

catObjectDetails :: CatFileHandle -> Ref -> IO (Maybe (L.ByteString, Sha, ObjectType))
catObjectDetails h object = query (catFileProcess h) object newlinefallback $ \from -> do
	header <- S8.hGetLine from
	case parseResp object header of
		Just r@(ParsedResp sha objtype _size) -> do
			content <- readObjectContent from r
			return $ Just (content, sha, objtype)
		Just DNE -> return Nothing
		Nothing -> error $ "unknown response from git cat-file " ++ show (header, object)
  where
	-- Slow fallback path for filenames containing newlines.
	newlinefallback = queryObjectType object (gitRepo h) >>= \case
		Nothing -> return Nothing
		Just objtype -> queryContent object (gitRepo h) >>= \case
			Nothing -> return Nothing
			Just content -> do
				-- only the --batch interface allows getting
				-- the sha, so have to re-hash the object
				sha <- hashObject' objtype
					(flip L.hPut content)
					(gitRepo h)
				return (Just (content, sha, objtype))

readObjectContent :: Handle -> ParsedResp -> IO L.ByteString
readObjectContent h (ParsedResp _ _ size) = do
	content <- S.hGet h (fromIntegral size)
	eatchar '\n'
	return (L.fromChunks [content])
  where
	eatchar expected = do
		c <- hGetChar h
		when (c /= expected) $
			error $ "missing " ++ (show expected) ++ " from git cat-file"
readObjectContent _ DNE = error "internal"

{- Gets the size and type of an object, without reading its content. -}
catObjectMetaData :: CatFileHandle -> Ref -> IO (Maybe (Sha, FileSize, ObjectType))
catObjectMetaData h object = query (checkFileProcess h) object newlinefallback $ \from -> do
	resp <- S8.hGetLine from
	case parseResp object resp of
		Just (ParsedResp sha objtype size) ->
			return $ Just (sha, size, objtype)
		Just DNE -> return Nothing
		Nothing -> error $ "unknown response from git cat-file " ++ show (resp, object)
  where
	-- Slow fallback path for filenames containing newlines.
	newlinefallback = do
		sha <- Git.Ref.sha object (gitRepo h)
		sz <- querySize object (gitRepo h)
		objtype <- queryObjectType object (gitRepo h)
		return $ (,,) <$> sha <*> sz <*> objtype

data ParsedResp = ParsedResp Sha ObjectType FileSize | DNE
	deriving (Show)

query :: CoProcess.CoProcessHandle -> Ref -> IO a -> (Handle -> IO a) -> IO a
query hdl object newlinefallback receive
	-- git cat-file --batch uses a line based protocol, so when the
	-- filename itself contains a newline, have to fall back to another
	-- method of getting the information.
	| '\n' `S8.elem` s = newlinefallback
	-- git strips carriage return from the end of a line, out of some
	-- misplaced desire to support windows, so also use the newline
	-- fallback for those.
	| "\r" `S8.isSuffixOf` s = newlinefallback
	| otherwise = CoProcess.query hdl send receive
  where
	send to = S8.hPutStrLn to s
	s = fromRef' object

parseResp :: Ref -> S.ByteString -> Maybe ParsedResp
parseResp object s
	| " missing" `S.isSuffixOf` s -- less expensive than full check
		&& s == fromRef' object <> " missing" = Just DNE
	| otherwise = eitherToMaybe $ A.parseOnly respParser s

respParser :: A.Parser ParsedResp
respParser = ParsedResp
	<$> (maybe (fail "bad sha") return . extractSha =<< nextword)
	<* A8.char ' '
	<*> (maybe (fail "bad object type") return . readObjectType =<< nextword)
	<* A8.char ' '
	<*> A8.decimal
  where
	nextword = A8.takeTill (== ' ')

querySingle :: CommandParam -> Ref -> Repo -> (Handle -> IO a) -> IO (Maybe a)
querySingle o r repo reader = assertLocal repo $
	-- In non-batch mode, git cat-file warns on stderr when
	-- asked for an object that does not exist.
	-- Squelch that warning to behave the same as batch mode.
	withNullHandle $ \nullh -> do
		let p = gitCreateProcess 
			[ Param "cat-file"
			, o
			, Param (fromRef r)
			] repo
		let p' = p
			{ std_err = UseHandle nullh
			, std_in = Inherit
			, std_out = CreatePipe
			}
		withCreateProcess p' go
  where
	go _ (Just outh) _ pid = do
		output <- reader outh
		hClose outh
		ifM (checkSuccessProcess pid)
			( return (Just output)
			, return Nothing
			)
	go _ _ _ _ = error "internal"

querySize :: Ref -> Repo -> IO (Maybe FileSize)
querySize r repo = maybe Nothing (readMaybe . takeWhile (/= '\n'))
	<$> querySingle (Param "-s") r repo hGetContentsStrict

queryObjectType :: Ref -> Repo -> IO (Maybe ObjectType)
queryObjectType r repo = maybe Nothing (readObjectType . encodeBS . takeWhile (/= '\n'))
	<$> querySingle (Param "-t") r repo hGetContentsStrict

queryContent :: Ref -> Repo -> IO (Maybe L.ByteString)
queryContent r repo = fmap (\b -> L.fromChunks [b])
	<$> querySingle (Param "-p") r repo S.hGetContents

{- Gets a list of files and directories in a tree. (Not recursive.) -}
catTree :: CatFileHandle -> Ref -> IO [(FilePath, FileMode)]
catTree h treeref = go <$> catObjectDetails h treeref
  where
	go (Just (b, _, TreeObject)) = parsetree [] b
	go _ = []

	parsetree c b = case L.break (== 0) b of
		(modefile, rest)
			| L.null modefile -> c
			| otherwise -> parsetree
				(parsemodefile modefile:c)
				(dropsha rest)

	-- these 20 bytes after the NUL hold the file's sha
	dropsha = L.drop 21

	parsemodefile b = 
		let (modestr, file) = separate (== ' ') (decodeBL b)
		in (file, readmode modestr)
	readmode = fromMaybe 0 . fmap fst . headMaybe . readOct

catCommit :: CatFileHandle -> Ref -> IO (Maybe Commit)
catCommit h commitref = go <$> catObjectDetails h commitref
  where
	go (Just (b, _, CommitObject)) = parseCommit (L.toStrict b)
	go _ = Nothing

parseCommit :: S.ByteString -> Maybe Commit
parseCommit b = Commit
	<$> (extractSha =<< field "tree")
	<*> Just (maybe [] (mapMaybe extractSha) (fields "parent"))
	<*> (parsemetadata <$> field "author")
	<*> (parsemetadata <$> field "committer")
	<*> Just (decodeBS $ S.intercalate (S.singleton nl) message)
  where
	field n = headMaybe =<< fields n
	fields n = M.lookup (fromString n) fieldmap
	fieldmap = M.fromListWith (++) ((map breakfield) header)
	breakfield l =
		let (k, sp_v) = S.break (== sp) l
		in (k, [S.drop 1 sp_v])
	(header, message) = separate S.null ls
	ls = S.split nl b

	-- author and committer lines have the form: "name <email> date"
	-- The email is always present, even if empty "<>"
	parsemetadata l = CommitMetaData
		{ commitName = whenset $ S.init name_sp
		, commitEmail = whenset email
		, commitDate = whenset $ S.drop 2 gt_sp_date
		}
	  where
		(name_sp, rest) = S.break (== lt) l
		(email, gt_sp_date) = S.break (== gt) (S.drop 1 rest)
		whenset v
			| S.null v = Nothing
			| otherwise = Just (decodeBS v)

	nl = fromIntegral (ord '\n')
	sp = fromIntegral (ord ' ')
	lt = fromIntegral (ord '<')
	gt = fromIntegral (ord '>')

{- Uses cat-file to stream the contents of the files as efficiently
 - as possible. This is much faster than querying it repeatedly per file.
 -}
catObjectStream
	:: (MonadMask m, MonadIO m)
	=> [LsTree.TreeItem]
	-> (LsTree.TreeItem -> Bool)
	-> Repo
	-> (IO (Maybe (TopFilePath, Maybe L.ByteString)) -> m ())
	-> m ()
catObjectStream l want repo a = withCatObjectStream repo feeder $
	\mv hout -> a (catObjectReader mv hout)
  where
	feeder mv h = do
		forM_ l $ \ti ->
			when (want ti) $ do
				let f = LsTree.file ti
				let sha = LsTree.sha ti
				liftIO $ atomically $ snocTList mv (sha, f)
				S8.hPutStrLn h (fromRef' sha)
		hClose h

{- While this variant could be combined with catObjectStream into a 
 - more polymorphic function, the specialization of both is important for
 - performance. -}
catObjectStream'
	:: (MonadMask m, MonadIO m)
	=> [(RawFilePath, Sha, FileMode)]
	-> Repo
	-> (IO (Maybe (RawFilePath, Maybe L.ByteString)) -> m ())
	-> m ()
catObjectStream' l repo a = withCatObjectStream repo feeder $
	\mv hout -> a (catObjectReader mv hout)
  where
	feeder mv h = do
		forM_ l $ \(f, sha, _) -> do
			liftIO $ atomically $ snocTList mv (sha, f)
			S8.hPutStrLn h (fromRef' sha)
		hClose h

catObjectReader :: TList (Ref, a) -> Handle -> IO (Maybe (a, Maybe L.ByteString))
catObjectReader mv h = ifM (hIsEOF h)
	( return Nothing
	, do
		(sha, f) <- liftIO $ atomically $ headTList mv
		resp <- S8.hGetLine h
		case parseResp sha resp of
			Just r@(ParsedResp {}) -> do
				content <- readObjectContent h r
				return (Just (f, Just content))
			Just DNE -> return (Just (f, Nothing))
			Nothing -> error $ "unknown response from git cat-file " ++ show resp
	)

withCatObjectStream
	:: (MonadMask m, MonadIO m)
	=> Repo
	-> (TList a -> Handle -> IO ())
	-> (TList a -> Handle -> m ())
	-> m ()
withCatObjectStream repo feeder reader = assertLocal repo $
	bracketIO start stop $ \(mv, _, _, hout, _) -> reader mv hout
  where
	params = 
		[ Param "cat-file"
		, Param ("--batch=" ++ batchFormat)
		, Param "--buffer"
		]

	start = do
		mv <- liftIO $ atomically newTList
		let p = gitCreateProcess params repo
		(Just hin, Just hout, _, pid) <- createProcess p
			{ std_in = CreatePipe
			, std_out = CreatePipe
			}
		f <- async (feeder mv hin)
		return (mv, f, hin, hout, pid)
	
	stop (_, f, hin, hout, pid) = do
		cancel f
		hClose hin
		hClose hout
		void $ checkSuccessProcess pid
