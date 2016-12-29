{- Construction of Git Repo objects
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Construct (
	fromCwd,
	fromAbsPath,
	fromPath,
	fromUrl,
	fromUnknown,
	localToUrl,
	remoteNamed,
	remoteNamedFromKey,
	isRemoteUrlConfig,
	fromRemotes,
	fromRemoteLocation,
	repoAbsPath,
	checkForRepo,
	newFrom,
) where

#ifndef mingw32_HOST_OS
import System.Posix.User
#endif
import qualified Data.Map as M hiding (map, split)
import Network.URI

import Common
import Git.Types
import Git
import Git.Remote
import Git.FilePath
import qualified Git.Url as Url
import Utility.UserInfo

{- Finds the git repository used for the cwd, which may be in a parent
 - directory. -}
fromCwd :: IO (Maybe Repo)
fromCwd = getCurrentDirectory >>= seekUp
  where
	seekUp dir = do
		r <- checkForRepo dir
		case r of
			Nothing -> case upFrom dir of
				Nothing -> return Nothing
				Just d -> seekUp d
			Just loc -> pure $ Just $ newFrom loc

{- Local Repo constructor, accepts a relative or absolute path. -}
fromPath :: FilePath -> IO Repo
fromPath dir = fromAbsPath =<< absPath dir

{- Local Repo constructor, requires an absolute path to the repo be
 - specified. -}
fromAbsPath :: FilePath -> IO Repo
fromAbsPath dir
	| absoluteGitPath dir = hunt
	| otherwise =
		error $ "internal error, " ++ dir ++ " is not absolute"
  where
	ret = pure . newFrom . LocalUnknown
	canondir = dropTrailingPathSeparator dir
	{- When dir == "foo/.git", git looks for "foo/.git/.git",
	 - and failing that, uses "foo" as the repository. -}
	hunt
		| (pathSeparator:".git") `isSuffixOf` canondir =
			ifM (doesDirectoryExist $ dir </> ".git")
				( ret dir
				, ret (takeDirectory canondir)
				)
		| otherwise = ifM (doesDirectoryExist dir)
			( ret dir
			-- git falls back to dir.git when dir doesn't
			-- exist, as long as dir didn't end with a
			-- path separator
			, if dir == canondir
				then ret (dir ++ ".git")
				else ret dir
			)

{- Remote Repo constructor. Throws exception on invalid url.
 -
 - Git is somewhat forgiving about urls to repositories, allowing
 - eg spaces that are not normally allowed unescaped in urls.
 -}
fromUrl :: String -> IO Repo
fromUrl url
	| not (isURI url) = fromUrlStrict $ escapeURIString isUnescapedInURI url
	| otherwise = fromUrlStrict url

fromUrlStrict :: String -> IO Repo
fromUrlStrict url
	| startswith "file://" url = fromAbsPath $ unEscapeString $ uriPath u
	| otherwise = pure $ newFrom $ Url u
  where
	u = fromMaybe bad $ parseURI url
	bad = error $ "bad url " ++ url

{- Creates a repo that has an unknown location. -}
fromUnknown :: Repo
fromUnknown = newFrom Unknown

{- Converts a local Repo into a remote repo, using the reference repo
 - which is assumed to be on the same host. -}
localToUrl :: Repo -> Repo -> Repo
localToUrl reference r
	| not $ repoIsUrl reference = error "internal error; reference repo not url"
	| repoIsUrl r = r
	| otherwise = case Url.authority reference of
		Nothing -> r
		Just auth -> 
			let absurl = concat
				[ Url.scheme reference
				, "//"
				, auth
				, repoPath r
				]
			in r { location = Url $ fromJust $ parseURI absurl }

{- Is a git config key the url of a remote? -}
isRemoteUrlConfig :: String -> Bool
isRemoteUrlConfig k = startswith "remote." k && endswith ".url" k

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
fromRemotes :: Repo -> IO [Repo]
fromRemotes repo = mapM construct remotepairs
  where
	filterconfig f = filter f $ M.toList $ config repo
	filterkeys f = filterconfig (\(k,_) -> f k)
	remotepairs = filterkeys isRemoteUrlConfig
	construct (k,v) = remoteNamedFromKey k $ fromRemoteLocation v repo

{- Sets the name of a remote when constructing the Repo to represent it. -}
remoteNamed :: String -> IO Repo -> IO Repo
remoteNamed n constructor = do
	r <- constructor
	return $ r { remoteName = Just n }

{- Sets the name of a remote based on the git config key, such as
 - "remote.foo.url". -}
remoteNamedFromKey :: String -> IO Repo -> IO Repo
remoteNamedFromKey k = remoteNamed basename
  where
	basename = intercalate "." $ 
		reverse $ drop 1 $ reverse $ drop 1 $ split "." k

{- Constructs a new Repo for one of a Repo's remotes using a given
 - location (ie, an url). -}
fromRemoteLocation :: String -> Repo -> IO Repo
fromRemoteLocation s repo = gen $ parseRemoteLocation s repo
  where
	gen (RemotePath p) = fromRemotePath p repo
	gen (RemoteUrl u) = fromUrl u

{- Constructs a Repo from the path specified in the git remotes of
 - another Repo. -}
fromRemotePath :: FilePath -> Repo -> IO Repo
fromRemotePath dir repo = do
	dir' <- expandTilde dir
	fromPath $ repoPath repo </> dir'

{- Git remotes can have a directory that is specified relative
 - to the user's home directory, or that contains tilde expansions.
 - This converts such a directory to an absolute path.
 - Note that it has to run on the system where the remote is.
 -}
repoAbsPath :: FilePath -> IO FilePath
repoAbsPath d = do
	d' <- expandTilde d
	h <- myHomeDir
	return $ h </> d'

expandTilde :: FilePath -> IO FilePath
#ifdef mingw32_HOST_OS
expandTilde = return
#else
expandTilde = expandt True
  where
	expandt _ [] = return ""
	expandt _ ('/':cs) = do
		v <- expandt True cs
		return ('/':v)
	expandt True ('~':'/':cs) = do
		h <- myHomeDir
		return $ h </> cs
	expandt True ('~':cs) = do
		let (name, rest) = findname "" cs
		u <- getUserEntryForName name
		return $ homeDirectory u </> rest
	expandt _ (c:cs) = do
		v <- expandt False cs
		return (c:v)
	findname n [] = (n, "")
	findname n (c:cs)
		| c == '/' = (n, cs)
		| otherwise = findname (n++[c]) cs
#endif

{- Checks if a git repository exists in a directory. Does not find
 - git repositories in parent directories. -}
checkForRepo :: FilePath -> IO (Maybe RepoLocation)
checkForRepo dir = 
	check isRepo $
		check gitDirFile $
			check isBareRepo $
				return Nothing
  where
	check test cont = maybe cont (return . Just) =<< test
	checkdir c = ifM c
		( return $ Just $ LocalUnknown dir
		, return Nothing
		)
	isRepo = checkdir $ gitSignature $ ".git" </> "config"
	isBareRepo = checkdir $ gitSignature "config"
		<&&> doesDirectoryExist (dir </> "objects")
	gitDirFile = do
		c <- firstLine <$>
			catchDefaultIO "" (readFile $ dir </> ".git")
		return $ if gitdirprefix `isPrefixOf` c
			then Just $ Local 
				{ gitdir = absPathFrom dir $
					drop (length gitdirprefix) c
				, worktree = Just dir
				}
			else Nothing
	  where
		gitdirprefix = "gitdir: "
	gitSignature file = doesFileExist $ dir </> file

newFrom :: RepoLocation -> Repo
newFrom l = Repo
	{ location = l
	, config = M.empty
	, fullconfig = M.empty
	, remotes = []
	, remoteName = Nothing
	, gitEnv = Nothing
	, gitEnvOverridesGitDir = False
	, gitGlobalOpts = []
	}

