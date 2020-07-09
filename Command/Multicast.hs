{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Multicast where

import Command
import Logs.Multicast
import Annex.Multicast
import Annex.WorkTree
import Annex.Content
import Annex.UUID
import Annex.Perms
import Utility.FileMode
#ifndef mingw32_HOST_OS
import Creds
#endif
import qualified Limit
import Types.FileMatcher
import qualified Git.LsFiles as LsFiles
import Utility.Hash
import Utility.Tmp
import Utility.Tmp.Dir
import Utility.Process.Transcript

import Data.Char
import qualified Data.ByteString.Lazy.UTF8 as B8
import qualified Data.Map as M
import Control.Concurrent.Async

cmd :: Command
cmd = command "multicast" SectionCommon "multicast file distribution"
	paramNothing (seek <$$> optParser)

data MultiCastAction
	= GenAddress
	| Send
	| Receive
	deriving (Show)

data MultiCastOptions = MultiCastOptions MultiCastAction [CommandParam] [FilePath]
	deriving (Show)

optParser :: CmdParamsDesc -> Parser MultiCastOptions
optParser _ = MultiCastOptions 
	<$> (genaddressp <|> sendp <|> receivep)
	<*> many uftpopt
	<*> cmdParams paramPaths
  where
	genaddressp = flag' GenAddress
		( long "gen-address"
		<> help "generate multicast encryption key and store address in git-annex branch"
		)
	sendp = flag' Send
		( long "send"
		<> help "multicast files"
		)
	receivep = flag' Receive
		( long "receive"
		<> help "listen for multicast files and store in repository"
		)
	uftpopt = Param <$> strOption
		( long "uftp-opt"
		<> short 'U'
		<> help "passed on to uftp/uftpd"
		<> metavar "OPTION"
		)

seek :: MultiCastOptions -> CommandSeek
seek (MultiCastOptions GenAddress _ _) = commandAction genAddress 
seek (MultiCastOptions Send ups fs) = commandAction $ send ups fs
seek (MultiCastOptions Receive ups []) = commandAction $ receive ups
seek (MultiCastOptions Receive _ _) = giveup "Cannot specify list of files with --receive; this receives whatever files the sender chooses to send."

genAddress :: CommandStart
genAddress = starting "gen-address" (ActionItemOther Nothing) $ do
	k <- uftpKey
	(s, ok) <- case k of
		KeyContainer s -> liftIO $ genkey (Param s)
		KeyFile f -> do
			createAnnexDirectory (takeDirectory f)
			liftIO $ nukeFile f
			liftIO $ protectedOutput $ genkey (File f)
	case (ok, parseFingerprint s) of
		(False, _) -> giveup $ "uftp_keymgt failed: " ++ s
		(_, Nothing) -> giveup $ "Failed to find fingerprint in uftp_keymgt output: " ++ s
		(True, Just fp) -> next $ do
			recordFingerprint fp =<< getUUID
			return True
  where
 	-- Annoyingly, the fingerprint is output to stderr.
	genkey p = processTranscript "uftp_keymgt" ps Nothing
	  where
		ps = toCommand $
			[ Param "-g"
			, keyparam
			, p
			]
	-- uftp only supports rsa up to 2048 which is on the lower
	-- limit of secure RSA key sizes. Instead, use an EC curve.
	-- Except for on Windows XP, secp521r1 is supported on all
	-- platforms by uftp. DJB thinks it's pretty good compared
	-- with other NIST curves: "there's one standard NIST curve
	-- using a nice prime, namely 2521−1  but the sheer size of this
	-- prime makes it much slower than NIST P-256"
	-- (http://blog.cr.yp.to/20140323-ecdsa.html)
	-- Since this key is only used to set up the block encryption,
	-- its slow speed is ok.
	keyparam = Param "ec:secp521r1"

parseFingerprint :: String -> Maybe Fingerprint
parseFingerprint = Fingerprint <$$> lastMaybe . filter isfingerprint . words 
  where
	isfingerprint s = 
		let os = filter (all isHexDigit) (splitc ':' s)
		in length os == 20
	
send :: [CommandParam] -> [FilePath] -> CommandStart
send ups fs = do
	-- Need to be able to send files with the names of git-annex
	-- keys, and uftp does not allow renaming the files that are sent.
	-- In a direct mode repository, the annex objects do not have
	-- the names of keys, and would have to be copied, which is too
	-- expensive.
	starting "sending files" (ActionItemOther Nothing) $
		withTmpFile "send" $ \t h -> do
			let ww = WarnUnmatchLsFiles
			fs' <- seekHelper id ww LsFiles.inRepo
				=<< workTreeItems ww fs
			matcher <- Limit.getMatcher
			let addlist f o = whenM (matcher $ MatchingFile $ FileInfo f f) $
				liftIO $ hPutStrLn h o
			forM_ fs' $ \f -> do
				mk <- lookupFile f
				case mk of
					Nothing -> noop
					Just k -> withObjectLoc k $
						addlist f . fromRawFilePath
			liftIO $ hClose h
			
			serverkey <- uftpKey
			u <- getUUID
			withAuthList $ \authlist -> do
				let ps =
					-- Force client authentication.
					[ Param "-c"
					, Param "-Y", Param "aes256-cbc"
					, Param "-h", Param "sha512"
					-- Picked ecdh_ecdsa for perfect forward secrecy,
					-- and because a EC key exchange algorithm is
					-- needed since all keys are EC.
					, Param "-e", Param "ecdh_ecdsa"
					, Param "-k", uftpKeyParam serverkey
					, Param "-U", Param (uftpUID u)
					-- only allow clients on the authlist
					, Param "-H", Param ("@"++authlist)
					-- pass in list of files to send
					, Param "-i", File t
					] ++ ups
				liftIO (boolSystem "uftp" ps) >>= showEndResult
			next $ return True

receive :: [CommandParam] -> CommandStart
receive ups = starting "receiving multicast files" (ActionItemOther Nothing) $ do
	showNote "Will continue to run until stopped by ctrl-c"
	
	showOutput
	clientkey <- uftpKey
	u <- getUUID
	(callback, environ, statush) <- liftIO multicastCallbackEnv
	tmpobjdir <- fromRepo gitAnnexTmpObjectDir
	createAnnexDirectory tmpobjdir
	withTmpDirIn tmpobjdir "multicast" $ \tmpdir -> withAuthList $ \authlist -> do
		abstmpdir <- liftIO $ absPath tmpdir
		abscallback <- liftIO $ searchPath callback
		let ps =
			-- Avoid it running as a daemon.
			[ Param "-d"
			-- Require encryption.
			, Param "-E"
			, Param "-k", uftpKeyParam clientkey
			, Param "-U", Param (uftpUID u)
			-- Only allow servers on the authlist
			, Param "-S", Param authlist
			-- Receive files into tmpdir
			-- (it needs an absolute path)
			, Param "-D", File abstmpdir
			-- Run callback after each file received
			-- (it needs an absolute path)
			, Param "-s", Param (fromMaybe callback abscallback)
			] ++ ups
		runner <- liftIO $ async $
			hClose statush
				`after` boolSystemEnv "uftpd" ps (Just environ)
		mapM_ storeReceived . lines =<< liftIO (hGetContents statush)
		showEndResult =<< liftIO (wait runner)
	next $ return True

storeReceived :: FilePath -> Annex ()
storeReceived f = do
	case deserializeKey (takeFileName f) of
		Nothing -> do
			warning $ "Received a file " ++ f ++ " that is not a git-annex key. Deleting this file."
			liftIO $ nukeFile f
		Just k -> void $
			getViaTmpFromDisk RetrievalVerifiableKeysSecure AlwaysVerify k $ \dest -> unVerified $
				liftIO $ catchBoolIO $ do
					rename f dest
					return True

-- Under Windows, uftp uses key containers, which are not files on the
-- filesystem.
data UftpKey = KeyFile FilePath | KeyContainer String

uftpKeyParam :: UftpKey -> CommandParam
uftpKeyParam (KeyFile f) = File f
uftpKeyParam (KeyContainer s) = Param s

uftpKey :: Annex UftpKey
#ifdef mingw32_HOST_OS
uftpKey = do
	u <- getUUID
	return $ KeyContainer $ "annex-" ++ fromUUID u
#else
uftpKey = KeyFile <$> credsFile "multicast"
#endif

-- uftp needs a unique UID for each client and server, which 
-- is a 8 digit hex number in the form "0xnnnnnnnn"
-- Derive it from the UUID.
uftpUID :: UUID -> String
uftpUID u = "0x" ++ (take 8 $ show $ sha2_256 $ B8.fromString (fromUUID u))

withAuthList :: (FilePath -> Annex a) -> Annex a
withAuthList a = do
	m <- knownFingerPrints
	withTmpFile "authlist" $ \t h -> do
		liftIO $ hPutStr h (genAuthList m)
		liftIO $ hClose h
		a t

genAuthList :: M.Map UUID Fingerprint -> String
genAuthList = unlines . map fmt . M.toList
  where
	fmt (u, Fingerprint f) = uftpUID u ++ "|" ++ f
