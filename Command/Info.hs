{- git-annex command
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}

module Command.Info where

import "mtl" Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Text.JSON
import Data.Ord

import Command
import qualified Git
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import Utility.DataUnits
import Utility.DiskFree
import Annex.Content
import Annex.UUID
import Logs.UUID
import Logs.Trust
import Logs.Location
import Annex.NumCopies
import Remote
import Utility.Percentage
import Logs.Transfer
import Types.TrustLevel
import Types.FileMatcher
import qualified Limit
import Messages.JSON (DualDisp(..))
import Annex.BloomFilter
import qualified Command.Unused

-- a named computation that produces a statistic
type Stat = StatState (Maybe (String, StatState String))

-- data about a set of keys
data KeyData = KeyData
	{ countKeys :: Integer
	, sizeKeys :: Integer
	, unknownSizeKeys :: Integer
	, backendsKeys :: M.Map String Integer
	}

data NumCopiesStats = NumCopiesStats
	{ numCopiesVarianceMap :: M.Map Variance Integer
	}

newtype Variance = Variance Int
	deriving (Eq, Ord)

instance Show Variance where
	show (Variance n)
		| n >= 0 = "+" ++ show n
		| otherwise = show n

-- cached info that multiple Stats use
data StatInfo = StatInfo
	{ presentData :: Maybe KeyData
	, referencedData :: Maybe KeyData
	, repoData :: M.Map UUID KeyData
	, numCopiesStats :: Maybe NumCopiesStats
	, infoOptions :: InfoOptions
	}

emptyStatInfo :: InfoOptions -> StatInfo
emptyStatInfo = StatInfo Nothing Nothing M.empty Nothing

-- a state monad for running Stats in
type StatState = StateT StatInfo Annex

cmd :: Command
cmd = noCommit $ withGlobalOptions (jsonOption : annexedMatchingOptions) $
	command "info" SectionQuery
		"shows  information about the specified item or the repository as a whole"
		(paramRepeating paramItem) (seek <$$> optParser)

data InfoOptions = InfoOptions
	{ infoFor :: CmdParams
	, bytesOption :: Bool
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser InfoOptions
optParser desc = InfoOptions
	<$> cmdParams desc
	<*> switch
		( long "bytes"
		<> help "display file sizes in bytes"
		)
	<*> parseBatchOption

seek :: InfoOptions -> CommandSeek
seek o = case batchOption o of
	NoBatch -> withWords (start o) (infoFor o)
	Batch -> batchInput Right (itemInfo o)

start :: InfoOptions -> [String] -> CommandStart
start o [] = do
	globalInfo o
	stop
start o ps = do
	mapM_ (itemInfo o) ps
	stop

globalInfo :: InfoOptions -> Annex ()
globalInfo o = do
	u <- getUUID
	whenM ((==) DeadTrusted <$> lookupTrust u) $
		earlyWarning "Warning: This repository is currently marked as dead."
	stats <- selStats global_fast_stats global_slow_stats
	showCustom "info" $ do
		evalStateT (mapM_ showStat stats) (emptyStatInfo o)
		return True

itemInfo :: InfoOptions -> String -> Annex ()
itemInfo o p = ifM (isdir p)
	( dirInfo o p
	, do
		v <- Remote.byName' p
		case v of
			Right r -> remoteInfo o r
			Left _ -> do
				v' <- Remote.nameToUUID' p
				case v' of
					Right u -> uuidInfo o u
					Left _ -> ifAnnexed p 
						(fileInfo o p)
						(noInfo p)
	)
  where
	isdir = liftIO . catchBoolIO . (isDirectory <$$> getFileStatus)

noInfo :: String -> Annex ()
noInfo s = do
	showStart "info" s
	showNote $ "not a directory or an annexed file or a remote or a uuid"
	showEndFail

dirInfo :: InfoOptions -> FilePath -> Annex ()
dirInfo o dir = showCustom (unwords ["info", dir]) $ do
	stats <- selStats (tostats dir_fast_stats) (tostats dir_slow_stats)
	evalStateT (mapM_ showStat stats) =<< getDirStatInfo o dir
	return True
  where
	tostats = map (\s -> s dir)

fileInfo :: InfoOptions -> FilePath -> Key -> Annex ()
fileInfo o file k = showCustom (unwords ["info", file]) $ do
	evalStateT (mapM_ showStat (file_stats file k)) (emptyStatInfo o)
	return True

remoteInfo :: InfoOptions -> Remote -> Annex ()
remoteInfo o r = showCustom (unwords ["info", Remote.name r]) $ do
	i <- map (\(k, v) -> simpleStat k (pure v)) <$> Remote.getInfo r
	l <- selStats (remote_fast_stats r ++ i) (uuid_slow_stats (Remote.uuid r))
	evalStateT (mapM_ showStat l) (emptyStatInfo o)
	return True

uuidInfo :: InfoOptions -> UUID -> Annex ()
uuidInfo o u = showCustom (unwords ["info", fromUUID u]) $ do
	l <- selStats [] ((uuid_slow_stats u))
	evalStateT (mapM_ showStat l) (emptyStatInfo o)
	return True

selStats :: [Stat] -> [Stat] -> Annex [Stat]
selStats fast_stats slow_stats = do
	fast <- Annex.getState Annex.fast
	return $ if fast
		then fast_stats
		else fast_stats ++ slow_stats

{- Order is significant. Less expensive operations, and operations
 - that share data go together.
 -}
global_fast_stats :: [Stat]
global_fast_stats = 
	[ repo_list Trusted
	, repo_list SemiTrusted
	, repo_list UnTrusted
	, transfer_list
	, disk_size
	]
global_slow_stats :: [Stat]
global_slow_stats = 
	[ tmp_size
	, bad_data_size
	, local_annex_keys
	, local_annex_size
	, known_annex_files
	, known_annex_size
	, bloom_info
	, backend_usage
	]
dir_fast_stats :: [FilePath -> Stat]
dir_fast_stats =
	[ dir_name
	, const local_annex_keys
	, const local_annex_size
	, const known_annex_files
	, const known_annex_size
	]
dir_slow_stats :: [FilePath -> Stat]
dir_slow_stats =
	[ const numcopies_stats
	, const reposizes_stats
	]

file_stats :: FilePath -> Key -> [Stat]
file_stats f k =
	[ file_name f
	, key_size k
	, key_name k
	]

remote_fast_stats :: Remote -> [Stat]
remote_fast_stats r = map (\s -> s r)
	[ remote_name
	, remote_description
	, remote_uuid
	, remote_trust
	, remote_cost
	, remote_type
	]

uuid_slow_stats :: UUID -> [Stat]
uuid_slow_stats u = map (\s -> s u)
	[ remote_annex_keys
	, remote_annex_size
	]

stat :: String -> (String -> StatState String) -> Stat
stat desc a = return $ Just (desc, a desc)

-- The json simply contains the same string that is displayed.
simpleStat :: String -> StatState String -> Stat
simpleStat desc getval = stat desc $ json id getval

nostat :: Stat
nostat = return Nothing

json :: JSON j => (j -> String) -> StatState j -> String -> StatState String
json fmt a desc = do
	j <- a
	lift $ maybeShowJSON [(desc, j)]
	return $ fmt j

nojson :: StatState String -> String -> StatState String
nojson a _ = a

showStat :: Stat -> StatState ()
showStat s = maybe noop calc =<< s
  where
	calc (desc, a) = do
		(lift . showHeader) desc
		lift . showRaw =<< a

repo_list :: TrustLevel -> Stat
repo_list level = stat n $ nojson $ lift $ do
	us <- filter (/= NoUUID) . M.keys 
		<$> (M.union <$> uuidMap <*> remoteMap Remote.name)
	rs <- fst <$> trustPartition level us
	countRepoList (length rs)
		-- This also handles json display.
		<$> prettyPrintUUIDs n rs
  where
	n = showTrustLevel level ++ " repositories"

countRepoList :: Int -> String -> String
countRepoList _ [] = "0"
countRepoList n s = show n ++ "\n" ++ beginning s

dir_name :: FilePath -> Stat
dir_name dir = simpleStat "directory" $ pure dir

file_name :: FilePath -> Stat
file_name file = simpleStat "file" $ pure file

remote_name :: Remote -> Stat
remote_name r = simpleStat "remote" $ pure (Remote.name r)

remote_description :: Remote -> Stat
remote_description r = simpleStat "description" $ lift $
	Remote.prettyUUID (Remote.uuid r)

remote_uuid :: Remote -> Stat
remote_uuid r = simpleStat "uuid" $ pure $
	fromUUID $ Remote.uuid r

remote_trust :: Remote -> Stat
remote_trust r = simpleStat "trust" $ lift $
	showTrustLevel <$> lookupTrust (Remote.uuid r)

remote_cost :: Remote -> Stat
remote_cost r = simpleStat "cost" $ pure $
	show $ Remote.cost r

remote_type :: Remote -> Stat
remote_type r = simpleStat "type" $ pure $
	Remote.typename $ Remote.remotetype r

local_annex_keys :: Stat
local_annex_keys = stat "local annex keys" $ json show $
	countKeys <$> cachedPresentData

local_annex_size :: Stat
local_annex_size = simpleStat "local annex size" $
	showSizeKeys =<< cachedPresentData

remote_annex_keys :: UUID -> Stat
remote_annex_keys u = stat "remote annex keys" $ json show $
	countKeys <$> cachedRemoteData u

remote_annex_size :: UUID -> Stat
remote_annex_size u = simpleStat "remote annex size" $
	showSizeKeys =<< cachedRemoteData u

known_annex_files :: Stat
known_annex_files = stat "annexed files in working tree" $ json show $
	countKeys <$> cachedReferencedData

known_annex_size :: Stat
known_annex_size = simpleStat "size of annexed files in working tree" $
	showSizeKeys =<< cachedReferencedData

tmp_size :: Stat
tmp_size = staleSize "temporary object directory size" gitAnnexTmpObjectDir

bad_data_size :: Stat
bad_data_size = staleSize "bad keys size" gitAnnexBadDir

key_size :: Key -> Stat
key_size k = simpleStat "size" $ showSizeKeys $ foldKeys [k]

key_name :: Key -> Stat
key_name k = simpleStat "key" $ pure $ key2file k

bloom_info :: Stat
bloom_info = simpleStat "bloom filter size" $ do
	localkeys <- countKeys <$> cachedPresentData
	capacity <- fromIntegral <$> lift bloomCapacity
	let note = aside $
		if localkeys >= capacity
		then "appears too small for this repository; adjust annex.bloomcapacity"
		else showPercentage 1 (percentage capacity localkeys) ++ " full"

	-- Two bloom filters are used at the same time when running
	-- git-annex unused, so double the size of one.
	sizer <- mkSizer
	size <- sizer memoryUnits False . (* 2) . fromIntegral . fst <$>
		lift bloomBitsHashes

	return $ size ++ note

transfer_list :: Stat
transfer_list = stat desc $ nojson $ lift $ do
	uuidmap <- Remote.remoteMap id
	ts <- getTransfers
	maybeShowJSON [(desc, map (uncurry jsonify) ts)]
	return $ if null ts
		then "none"
		else multiLine $
			map (uncurry $ line uuidmap) $ sort ts
  where
	desc = "transfers in progress"
	line uuidmap t i = unwords
		[ showLcDirection (transferDirection t) ++ "ing"
		, fromMaybe (key2file $ transferKey t) (associatedFile i)
		, if transferDirection t == Upload then "to" else "from"
		, maybe (fromUUID $ transferUUID t) Remote.name $
			M.lookup (transferUUID t) uuidmap
		]
	jsonify t i = toJSObject
		[ ("transfer", showLcDirection (transferDirection t))
		, ("key", key2file (transferKey t))
		, ("file", fromMaybe "" (associatedFile i))
		, ("remote", fromUUID (transferUUID t))
		]

disk_size :: Stat
disk_size = simpleStat "available local disk space" $
	calcfree
		<$> (lift $ annexDiskReserve <$> Annex.getGitConfig)
		<*> (lift $ inRepo $ getDiskFree . gitAnnexDir)
		<*> mkSizer
  where
	calcfree reserve (Just have) sizer = unwords
		[ sizer storageUnits False $ nonneg $ have - reserve
		, "(+" ++ sizer storageUnits False reserve
		, "reserved)"
		]			
	calcfree _ _ _ = "unknown"

	nonneg x
		| x >= 0 = x
		| otherwise = 0

backend_usage :: Stat
backend_usage = stat "backend usage" $ json fmt $
	toJSObject . sort . M.toList . backendsKeys <$> cachedReferencedData
  where
	fmt = multiLine . map (\(b, n) -> b ++ ": " ++ show n) . fromJSObject

numcopies_stats :: Stat
numcopies_stats = stat "numcopies stats" $ json fmt $
	calc <$> (maybe M.empty numCopiesVarianceMap <$> cachedNumCopiesStats)
  where
	calc = map (\(variance, count) -> (show variance, count)) 
		. sortBy (flip (comparing snd))
		. M.toList
	fmt = multiLine . map (\(variance, count) -> "numcopies " ++ variance ++ ": " ++ show count)

reposizes_stats :: Stat
reposizes_stats = stat desc $ nojson $ do
	sizer <- mkSizer
	l <- map (\(u, kd) -> (u, sizer storageUnits True (sizeKeys kd)))
		. sortBy (flip (comparing (sizeKeys . snd)))
		. M.toList
		<$> cachedRepoData
	let maxlen = maximum (map (length . snd) l)
	descm <- lift uuidDescriptions
	-- This also handles json display.
	s <- lift $ prettyPrintUUIDsWith (Just "size") desc descm (Just . show) $
		map (\(u, sz) -> (u, Just $ mkdisp sz maxlen)) l
	return $ countRepoList (length l) s
  where
	desc = "repositories containing these files"
	mkdisp sz maxlen = DualDisp
		{ dispNormal = lpad maxlen sz
		, dispJson = sz
		}
	lpad n s = (replicate (n - length s) ' ') ++ s

cachedPresentData :: StatState KeyData
cachedPresentData = do
	s <- get
	case presentData s of
		Just v -> return v
		Nothing -> do
			v <- foldKeys <$> lift (getKeysPresent InRepository)
			put s { presentData = Just v }
			return v

cachedRemoteData :: UUID -> StatState KeyData
cachedRemoteData u = do
	s <- get
	case M.lookup u (repoData s) of
		Just v -> return v
		Nothing -> do
			v <- foldKeys <$> lift (loggedKeysFor u)
			put s { repoData = M.insert u v (repoData s) }
			return v

cachedReferencedData :: StatState KeyData
cachedReferencedData = do
	s <- get
	case referencedData s of
		Just v -> return v
		Nothing -> do
			!v <- lift $ Command.Unused.withKeysReferenced
				emptyKeyData addKey
			put s { referencedData = Just v }
			return v

-- currently only available for directory info
cachedNumCopiesStats :: StatState (Maybe NumCopiesStats)
cachedNumCopiesStats = numCopiesStats <$> get

-- currently only available for directory info
cachedRepoData :: StatState (M.Map UUID KeyData)
cachedRepoData = repoData <$> get

getDirStatInfo :: InfoOptions -> FilePath -> Annex StatInfo
getDirStatInfo o dir = do
	fast <- Annex.getState Annex.fast
	matcher <- Limit.getMatcher
	(presentdata, referenceddata, numcopiesstats, repodata) <-
		Command.Unused.withKeysFilesReferencedIn dir initial
			(update matcher fast)
	return $ StatInfo (Just presentdata) (Just referenceddata) repodata (Just numcopiesstats) o
  where
	initial = (emptyKeyData, emptyKeyData, emptyNumCopiesStats, M.empty)
	update matcher fast key file vs@(presentdata, referenceddata, numcopiesstats, repodata) =
		ifM (matcher $ MatchingFile $ FileInfo file file)
			( do
				!presentdata' <- ifM (inAnnex key)
					( return $ addKey key presentdata
					, return presentdata
					)
				let !referenceddata' = addKey key referenceddata
				(!numcopiesstats', !repodata') <- if fast
					then return (numcopiesstats, repodata)
					else do
						locs <- Remote.keyLocations key
						nc <- updateNumCopiesStats file numcopiesstats locs
						return (nc, updateRepoData key locs repodata)
				return $! (presentdata', referenceddata', numcopiesstats', repodata')
			, return vs
			)

emptyKeyData :: KeyData
emptyKeyData = KeyData 0 0 0 M.empty

emptyNumCopiesStats :: NumCopiesStats
emptyNumCopiesStats = NumCopiesStats M.empty

foldKeys :: [Key] -> KeyData
foldKeys = foldl' (flip addKey) emptyKeyData

addKey :: Key -> KeyData -> KeyData
addKey key (KeyData count size unknownsize backends) =
	KeyData count' size' unknownsize' backends'
  where
	{- All calculations strict to avoid thunks when repeatedly
	 - applied to many keys. -}
	!count' = count + 1
	!backends' = M.insertWith (+) (keyBackendName key) 1 backends
	!size' = maybe size (+ size) ks
	!unknownsize' = maybe (unknownsize + 1) (const unknownsize) ks
	ks = keySize key

updateRepoData :: Key -> [UUID] -> M.Map UUID KeyData -> M.Map UUID KeyData
updateRepoData key locs m = m'
  where
	!m' = M.unionWith (\_old new -> new) m $
		M.fromList $ zip locs (map update locs)
	update loc = addKey key (fromMaybe emptyKeyData $ M.lookup loc m)

updateNumCopiesStats :: FilePath -> NumCopiesStats -> [UUID] -> Annex NumCopiesStats
updateNumCopiesStats file (NumCopiesStats m) locs = do
	have <- trustExclude UnTrusted locs
	!variance <- Variance <$> numCopiesCheck' file (-) have
	let !m' = M.insertWith (+) variance 1 m
	let !ret = NumCopiesStats m'
	return ret

showSizeKeys :: KeyData -> StatState String
showSizeKeys d = do
	sizer <- mkSizer
	return $ total sizer ++ missingnote
  where
	total sizer = sizer storageUnits False $ sizeKeys d
	missingnote
		| unknownSizeKeys d == 0 = ""
		| otherwise = aside $
			"+ " ++ show (unknownSizeKeys d) ++
			" unknown size"

staleSize :: String -> (Git.Repo -> FilePath) -> Stat
staleSize label dirspec = go =<< lift (dirKeys dirspec)
  where
	go [] = nostat
	go keys = onsize =<< sum <$> keysizes keys
	onsize 0 = nostat
	onsize size = stat label $
		json (++ aside "clean up with git-annex unused") $ do
			sizer <- mkSizer
			return $ sizer storageUnits False size
	keysizes keys = do
		dir <- lift $ fromRepo dirspec
		liftIO $ forM keys $ \k -> catchDefaultIO 0 $
			getFileSize (dir </> keyFile k)

aside :: String -> String
aside s = " (" ++ s ++ ")"

multiLine :: [String] -> String
multiLine = concatMap (\l -> "\n\t" ++ l)

mkSizer :: StatState ([Unit] -> Bool -> ByteSize -> String)
mkSizer = ifM (bytesOption . infoOptions <$> get)
	( return (const $ const show)
	, return roughSize
	)
