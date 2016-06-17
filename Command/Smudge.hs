{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Smudge where

import Command
import qualified Annex
import Annex.Content
import Annex.Link
import Annex.FileMatcher
import Annex.Ingest
import Annex.CatFile
import Logs.Location
import qualified Database.Keys
import Git.FilePath
import Backend

import qualified Data.ByteString.Lazy as B

cmd :: Command
cmd = noCommit $ noMessages $
	command "smudge" SectionPlumbing 
		"git smudge filter"
		paramFile (seek <$$> optParser)

data CmdOptions = CmdOptions
	{ workTreeFile :: FilePath
	, cleanOption :: Bool
	, directAccessOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser CmdOptions
optParser desc = CmdOptions
	<$> argument str ( metavar desc )
	<*> switch ( long "clean" <> help "clean filter" )
	<*> switch ( long "direct-access" <> help "direct file access (for smudgeToFile and cleanFromFile)")

seek :: CmdOptions -> CommandSeek
seek o = commandAction $
	(if cleanOption o then clean else smudge) o

-- Smudge filter is fed git file content on stdin, and if it's a
-- pointer to an available annex object, should provide its content.
smudge :: CmdOptions -> CommandStart
smudge o = do
	b <- liftIO $ B.hGetContents stdin
	case parseLinkOrPointer b of
		Nothing -> providecontent b
		Just k -> do
			Database.Keys.addAssociatedFile k =<< inRepo (toTopFilePath file)
			-- A previous unlocked checkout of the file may have
			-- led to the annex object getting modified;
			-- don't provide such modified content as it
			-- will be confusing. inAnnex will detect such
			-- modifications.
			ifM (inAnnex k)
				( do
					contentfile <- calcRepo (gitAnnexLocation k)
					thin <- annexThin <$> Annex.getGitConfig
					if thin && directAccessOption o
						then do
							r <- linkFromAnnex k file Nothing
							case r of
								LinkAnnexFailed -> providecontent b
								_ -> return ()
						else do
							when thin $
								warning $ "Not able to honor annex.thin when git is checking out " ++ file ++ " (run git annex fix to re-thin files)"
							providecontent . fromMaybe b
								=<< liftIO (catchMaybeIO (B.readFile contentfile))
				, providecontent b
				)
	stop
  where
	providecontent b
		| directAccessOption o = liftIO $ B.writeFile file b
		| otherwise = liftIO $ B.putStr b
	file = workTreeFile o

-- Clean filter is fed file content, decides if a file should be stored in
-- the annex, and outputs a pointer to its injested content.
clean :: CmdOptions -> CommandStart
clean o = do
	b <- liftIO $ if directAccessOption o
		then B.readFile file
		else B.hGetContents stdin
	if isJust (parseLinkOrPointer b)
		then liftIO $ B.hPut stdout b
		else ifM (shouldAnnex file)
			( do
				-- Even though we ingest the worktree file,
				-- and not stdin, we need to consume all
				-- stdin, or git will get annoyed.
				unless (directAccessOption o) $
					B.length b `seq` return ()
				-- Look up the backend that was used
				-- for this file before, so that when
				-- git re-cleans a file its backend does
				-- not change.
				currbackend <- maybe Nothing (maybeLookupBackendName . keyBackendName)
					<$> catKeyFile file
				-- Ingesting the worktree file is really
				-- only safe when git runs the smudgeToFile
				-- filter. But in practice, the smudge
				-- filter is only run on worktree files,
				-- so we can get away with directly
				-- accessing the file.
				liftIO . emitPointer
					=<< go
					=<< ingest' currbackend
					=<< lockDown cfg file
			, liftIO $ B.hPut stdout b
			)
	stop
  where
	file = workTreeFile o
	go (Just k, _) = do
		logStatus k InfoPresent
		return k
	go _ = error "could not add file to the annex"
	cfg = LockDownConfig
		{ lockingFile = False
		, hardlinkFileTmp = False
		}

shouldAnnex :: FilePath -> Annex Bool
shouldAnnex file = do
	matcher <- largeFilesMatcher
	checkFileMatcher matcher file

emitPointer :: Key -> IO ()
emitPointer = putStr . formatPointer
