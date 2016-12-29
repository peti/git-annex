{- git-annex command
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Map where

import qualified Data.Map as M

import Command
import qualified Git
import qualified Git.Url
import qualified Git.Config
import qualified Git.Construct
import qualified Remote
import qualified Annex
import Annex.UUID
import Logs.UUID
import Logs.Trust
import Types.TrustLevel
import qualified Remote.Helper.Ssh as Ssh
import qualified Utility.Dot as Dot
import P2P.Address
import P2P.Auth
import qualified P2P.IO as P2P
import qualified P2P.Annex as P2P
import qualified P2P.Protocol as P2P

-- a link from the first repository to the second (its remote)
data Link = Link Git.Repo Git.Repo

cmd :: Command
cmd = dontCheck repoExists $
	command "map" SectionQuery
		"generate map of repositories"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	rs <- combineSame <$> (spider =<< gitRepo)

	umap <- uuidMap
	trustmap <- trustMapLoad
		
	file <- (</>) <$> fromRepo gitAnnexDir <*> pure "map.dot"

	liftIO $ writeFile file (drawMap rs trustmap umap)
	next $ next $
		ifM (Annex.getState Annex.fast)
			( runViewer file []
			, runViewer file
	 			[ ("xdot", [File file])
				, ("dot", [Param "-Tx11", File file])
				]	
			)

runViewer :: FilePath -> [(String, [CommandParam])] -> Annex Bool
runViewer file [] = do
	showLongNote $ "left map in " ++ file
	return True
runViewer file ((c, ps):rest) = ifM (liftIO $ inPath c)
	( do
		showLongNote $ "running: " ++ c ++ unwords (toCommand ps)
		showOutput
		liftIO $ boolSystem c ps
	, runViewer file rest
	)

{- Generates a graph for dot(1). Each repository, and any other uuids
 - (except for dead ones), are displayed as a node, and each of its
 - remotes is represented as an edge pointing at the node for the remote.
 -
 - The order nodes are added to the graph matters, since dot will draw
 - the first ones near to the top and left. So it looks better to put
 - the repositories first, followed by uuids that were not matched
 - to a repository.
 -}
drawMap :: [Git.Repo] -> TrustMap -> M.Map UUID String -> String
drawMap rs trustmap umap = Dot.graph $ repos ++ others
  where
	repos = map (node umap rs trustmap) rs
	ruuids = map getUncachedUUID rs
	others = map uuidnode $ 
		filter (\u -> M.lookup u trustmap /= Just DeadTrusted) $
		filter (`notElem` ruuids) (M.keys umap)
	uuidnode u = trustDecorate trustmap u $
		Dot.graphNode (fromUUID u) $ M.findWithDefault "" u umap

hostname :: Git.Repo -> String
hostname r
	| Git.repoIsUrl r = fromMaybe (Git.repoLocation r) (Git.Url.host r)
	| otherwise = "localhost"

basehostname :: Git.Repo -> String
basehostname r = fromMaybe "" $ headMaybe $ split "." $ hostname r

{- A name to display for a repo. Uses the name from uuid.log if available,
 - or the remote name if not. -}
repoName :: M.Map UUID String -> Git.Repo -> String
repoName umap r
	| repouuid == NoUUID = fallback
	| otherwise = M.findWithDefault fallback repouuid umap
  where
	repouuid = getUncachedUUID r
	fallback = fromMaybe "unknown" $ Git.remoteName r

{- A unique id for the node for a repo. Uses the annex.uuid if available. -}
nodeId :: Git.Repo -> String
nodeId r =
	case getUncachedUUID r of
		NoUUID -> Git.repoLocation r
		UUID u -> u

{- A node representing a repo. -}
node :: M.Map UUID String -> [Git.Repo] -> TrustMap -> Git.Repo -> String
node umap fullinfo trustmap r = unlines $ n:edges
  where
	n = Dot.subGraph (hostname r) (basehostname r) "lightblue" $
		trustDecorate trustmap (getUncachedUUID r) $
			Dot.graphNode (nodeId r) (repoName umap r)
	edges = map (edge umap fullinfo r) (Git.remotes r)

{- An edge between two repos. The second repo is a remote of the first. -}
edge :: M.Map UUID String -> [Git.Repo] -> Git.Repo -> Git.Repo -> String	
edge umap fullinfo from to =
	Dot.graphEdge (nodeId from) (nodeId fullto) edgename
  where
	-- get the full info for the remote, to get its UUID
	fullto = findfullinfo to
	findfullinfo n =
		case filter (same n) fullinfo of
			[] -> n
			(n':_) -> n'
	{- Only name an edge if the name is different than the name
	 - that will be used for the destination node, and is
	 - different from its hostname. (This reduces visual clutter.) -}
	edgename = maybe Nothing calcname $ Git.remoteName to
	calcname n
		| n `elem` [repoName umap fullto, hostname fullto] = Nothing
		| otherwise = Just n

trustDecorate :: TrustMap -> UUID -> String -> String
trustDecorate trustmap u s = case M.lookup u trustmap of
	Just Trusted -> Dot.fillColor "green" s
	Just UnTrusted -> Dot.fillColor "red" s
	Just SemiTrusted -> Dot.fillColor "white" s
	Just DeadTrusted -> Dot.fillColor "grey" s
	Nothing -> Dot.fillColor "white" s

{- Recursively searches out remotes starting with the specified repo. -}
spider :: Git.Repo -> Annex [Git.Repo]
spider r = spider' [r] []
spider' :: [Git.Repo] -> [Git.Repo] -> Annex [Git.Repo]
spider' [] known = return known
spider' (r:rs) known
	| any (same r) known = spider' rs known
	| otherwise = do
		r' <- scan r

		-- The remotes will be relative to r', and need to be
		-- made absolute for later use.
		remotes <- mapM (absRepo r') (Git.remotes r')
		let r'' = r' { Git.remotes = remotes }

		spider' (rs ++ remotes) (r'':known)

{- Converts repos to a common absolute form. -}
absRepo :: Git.Repo -> Git.Repo -> Annex Git.Repo
absRepo reference r
	| Git.repoIsUrl reference = return $ Git.Construct.localToUrl reference r
	| Git.repoIsUrl r = return r
	| otherwise = liftIO $ do
		r' <- Git.Construct.fromAbsPath =<< absPath (Git.repoPath r)
		r'' <- safely $ flip Annex.eval Annex.gitRepo =<< Annex.new r'
		return (fromMaybe r' r'')

{- Checks if two repos are the same. -}
same :: Git.Repo -> Git.Repo -> Bool
same a b
	| both Git.repoIsUrl = matching Git.Url.scheme && matching Git.Url.authority && matching Git.repoPath
	| neither Git.repoIsUrl = matching Git.repoPath
	| otherwise = False
  where
	matching t = t a == t b
	both t = t a && t b
	neither t = not (t a) && not (t b)

{- reads the config of a remote, with progress display -}
scan :: Git.Repo -> Annex Git.Repo
scan r = do
	showStart "map" $ Git.repoDescribe r
	v <- tryScan r
	case v of
		Just r' -> do
			showEndOk
			return r'
		Nothing -> do
			showOutput
			showEndFail
			return r

{- tries to read the config of a remote, returning it only if it can
 - be accessed -}
tryScan :: Git.Repo -> Annex (Maybe Git.Repo)
tryScan r
	| Git.repoIsSsh r = sshscan
	| Git.repoIsUrl r = case repoP2PAddress r of
		Just addr -> either (const noscan) return
			=<< tryNonAsync (p2pscan addr)
		Nothing -> noscan -- can't scan an arbitrary url
	| otherwise = liftIO $ safely $ Git.Config.read r
  where
	pipedconfig pcmd params = liftIO $ safely $
		withHandle StdoutHandle createProcessSuccess p $
			Git.Config.hRead r
	  where
		p = proc pcmd $ toCommand params

	configlist = Ssh.onRemote r (pipedconfig, return Nothing) "configlist" [] []
	manualconfiglist = do
		gc <- Annex.getRemoteGitConfig r
		sshparams <- Ssh.toRepo r gc [Param sshcmd]
		liftIO $ pipedconfig "ssh" sshparams
	  where
		sshcmd = "sh -c " ++ shellEscape
			(cddir ++ " && " ++ "git config --null --list")
		dir = Git.repoPath r
		cddir
			| "/~" `isPrefixOf` dir =
				let (userhome, reldir) = span (/= '/') (drop 1 dir)
				in "cd " ++ userhome ++ " && " ++ cdto (drop 1 reldir)
			| otherwise = cdto dir
		cdto p = "if ! cd " ++ shellEscape p ++ " 2>/dev/null; then cd " ++ shellEscape p ++ ".git; fi"

	noscan = case Git.remoteName r of
		Just n -> Just <$> (either
			(const (pure r))
			(liftIO . setUUID r . Remote.uuid)
			=<< Remote.byName' n)
		Nothing -> return $ Just r

	-- First, try sshing and running git config manually,
	-- only fall back to git-annex-shell configlist if that
	-- fails.
	-- 
	-- This is done for two reasons, first I'd like this
	-- subcommand to be usable on non-git-annex repos.
	-- Secondly, configlist doesn't include information about
	-- the remote's remotes.
	sshscan = do
		sshnote
		v <- manualconfiglist
		case v of
			Nothing -> do
				sshnote
				configlist
			ok -> return ok

	sshnote = do
		showAction "sshing"
		showOutput

	p2pscan addr = do
		v <- loadP2PRemoteAuthToken addr
		case v of
			Nothing -> noscan
			Just authtoken -> do
				conn <- inRepo $ flip P2P.connectPeer addr
				myuuid <- getUUID
				res <- P2P.runFullProto P2P.Client conn $ do
					_ <- P2P.auth myuuid authtoken
					P2P.configList
				case res of
					Left _ -> noscan
					Right ls -> liftIO $ Just
						<$> Git.Config.store (unlines ls) r

{- Spidering can find multiple paths to the same repo, so this is used
 - to combine (really remove) duplicate repos with the same UUID. -}
combineSame :: [Git.Repo] -> [Git.Repo]
combineSame = map snd . nubBy sameuuid . map pair
  where
	sameuuid (u1, _) (u2, _) = u1 == u2 && u1 /= NoUUID
	pair r = (getUncachedUUID r, r)

safely :: IO Git.Repo -> IO (Maybe Git.Repo)
safely a = do
	result <- tryNonAsync a
	case result of
		Left _ -> return Nothing
		Right r' -> return $ Just r'
