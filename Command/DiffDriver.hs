{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DiffDriver where

import Command
import Annex.Content
import Annex.Link
import Git.Types

cmd :: Command
cmd = dontCheck repoExists $
	command "diffdriver" SectionPlumbing 
		"external git diff driver shim"
		("-- cmd --") (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start opts = do
	let (req, differ) = parseReq opts
	void $ liftIO . exitBool =<< liftIO . differ =<< fixupReq req
	stop

data Req 
	= Req
		{ rPath :: FilePath
		, rOldFile :: FilePath
		, rOldHex :: String
		, rOldMode :: String
		, rNewFile :: FilePath
		, rNewHex :: String
		, rNewMode ::String
		}
	| UnmergedReq
		{ rPath :: FilePath
		}

type Differ = Req -> IO Bool

serializeReq :: Req -> [CommandParam]
serializeReq req@(UnmergedReq {}) = [Param $ rPath req]
serializeReq req@(Req {}) = map Param
	[ rPath req
	, rOldFile req
	, rOldHex req
	, rOldMode req
	, rNewFile req
	, rNewHex req
	, rNewMode req
	]

parseReq :: [String] -> (Req, Differ)
parseReq opts = case separate (== "--") opts of
	(c:ps, l) -> (mk l, externalDiffer c ps)
	([],_) -> badopts
  where
	mk (path:old_file:old_hex:old_mode:new_file:new_hex:new_mode:[]) =
		Req
			{ rPath = path
			, rOldFile = old_file
			, rOldHex = old_hex
			, rOldMode = old_mode
			, rNewFile = new_file
			, rNewHex = new_hex
			, rNewMode = new_mode
			}
	mk (unmergedpath:[]) = UnmergedReq { rPath = unmergedpath }
	mk _ = badopts

	badopts = error $ "Unexpected input: " ++ unwords opts

{- Check if either file is a symlink to a git-annex object,
 - which git-diff will leave as a normal file containing the link text.
 - Adjust the Req to instead point to the actual location of the annexed
 - object (which may or may not exist). -}
fixupReq :: Req -> Annex Req
fixupReq req@(UnmergedReq {}) = return req
fixupReq req@(Req {}) = 
	check rOldFile rOldMode (\r f -> r { rOldFile = f }) req
		>>= check rNewFile rNewMode (\r f -> r { rNewFile = f })
  where
	check getfile getmode setfile r = case readBlobType (getmode r) of
		Just SymlinkBlob -> do
			v <- getAnnexLinkTarget' (getfile r) False
			case fileKey . takeFileName =<< v of
				Nothing -> return r
				Just k -> setfile r <$> withObjectLoc k return
		_ -> return r

externalDiffer :: String -> [String] -> Differ
externalDiffer c ps = \req -> boolSystem c (map Param ps ++ serializeReq req )
