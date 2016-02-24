{- git-annex command
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Status where

import Command
import Annex.CatFile
import Git.Status
import qualified Git.Ref
import Git.FilePath

cmd :: Command
cmd = notBareRepo $ noCommit $ noMessages $
	withGlobalOptions [jsonOption] $
		command "status" SectionCommon
			"show the working tree status"
			paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start
	
start :: [FilePath] -> CommandStart
start locs = do
	(l, cleanup) <- inRepo $ getStatus locs
	getstatus <- return $ \s -> pure (Just s)
	forM_ l $ \s -> maybe noop displayStatus =<< getstatus s
	void $ liftIO cleanup
	stop

displayStatus :: Status -> Annex ()
-- renames not shown in this simplified status
displayStatus (Renamed _ _) = noop
displayStatus s  = do
	let c = statusChar s
	absf <- fromRepo $ fromTopFilePath (statusFile s)
	f <- liftIO $ relPathCwdToFile absf
	unlessM (showFullJSON [("status", [c]), ("file", f)]) $
		liftIO $ putStrLn $ [c] ++ " " ++ f

checkNew :: FilePath -> TopFilePath -> Annex Status
checkNew f t = ifM (isJust <$> catObjectDetails (Git.Ref.fileRef f))
	( return (Modified t)
	, return (Untracked t)
	)
