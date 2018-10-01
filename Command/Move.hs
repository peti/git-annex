{- git-annex command
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Move where

import Command
import qualified Command.Drop
import qualified Annex
import Annex.Content
import qualified Remote
import Annex.UUID
import Annex.Transfer
import Logs.Presence
import Logs.Trust
import Annex.NumCopies

import System.Log.Logger (debugM)

cmd :: Command
cmd = withGlobalOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "move" SectionCommon
		"move content of files to/from another repository"
		paramPaths (seek <--< optParser)

data MoveOptions = MoveOptions
	{ moveFiles :: CmdParams
	, fromToOptions :: FromToHereOptions [DeferredParse Remote]
	, removeWhen :: RemoveWhen
	, keyOptions :: Maybe KeyOptions
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser MoveOptions
optParser desc = MoveOptions
	<$> cmdParams desc
	<*> parseFromToHereOptions
	<*> pure RemoveSafe
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseBatchOption

instance DeferredParseClass MoveOptions where
	finishParse v = MoveOptions
		<$> pure (moveFiles v)
		<*> finishParse (fromToOptions v)
		<*> pure (removeWhen v)
		<*> pure (keyOptions v)
		<*> pure (batchOption v)

data RemoveWhen = RemoveSafe | RemoveNever
	deriving (Show, Eq)

seek :: MoveOptions -> CommandSeek
seek o = allowConcurrentOutput $
	case batchOption o of
		Batch fmt -> batchFilesMatching fmt $ \f -> seekremotes' $ \r -> go r f
		NoBatch -> withKeyOptions (keyOptions o) False
			(\kai -> seekremotes $ \r -> commandAction $ startKey r (removeWhen o) kai)
			(withFilesInGit $ \f -> seekremotes $ \r -> commandAction $ go r f)
			=<< workTreeItems (moveFiles o)
  where
	go r = whenAnnexed $ start r (removeWhen o)

	seekremotes :: (FromToHereOptions Remote -> Annex ()) -> Annex ()
	seekremotes a = case fromToOptions o of
		Right (From rs) -> mapM_ (a . Right . From) =<< mapM getParsed rs
		Right (To rs) -> mapM_ (a . Right . To) =<< mapM getParsed rs
		Left ToHere -> a $ Left ToHere
	seekremotes' = undefined

start :: FromToHereOptions Remote -> RemoveWhen -> FilePath -> Key -> CommandStart
start fromto removewhen f k =
	start' fromto removewhen afile k (mkActionItem afile)
  where
	afile = AssociatedFile (Just f)

startKey :: FromToHereOptions Remote -> RemoveWhen -> (Key, ActionItem) -> CommandStart
startKey fromto removewhen = 
	uncurry $ start' fromto removewhen (AssociatedFile Nothing)

start' :: FromToHereOptions Remote -> RemoveWhen -> AssociatedFile -> Key -> ActionItem -> CommandStart
start' fromto removewhen afile key ai = onlyActionOn key $
	case fromto of
		Right (From src) ->
			checkFailedTransferDirection ai Download $
				fromStart removewhen afile key ai src
		Right (To dest) ->
			checkFailedTransferDirection ai Upload $
				toStart removewhen afile key ai dest
		Left ToHere ->
			checkFailedTransferDirection ai Download $
				toHereStart removewhen afile key ai

showMoveAction :: RemoveWhen -> Key -> ActionItem -> Annex ()
showMoveAction RemoveNever = showStartKey "copy"
showMoveAction _ = showStartKey "move"

toStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> Remote -> CommandStart
toStart removewhen afile key ai dest = do
	u <- getUUID
	ishere <- inAnnex key
	if not ishere || u == Remote.uuid dest
		then stop -- not here, so nothing to do
		else toStart' dest removewhen afile key ai

toStart' :: Remote -> RemoveWhen -> AssociatedFile -> Key -> ActionItem -> CommandStart
toStart' dest removewhen afile key ai = do
	fast <- Annex.getState Annex.fast
	if fast && removewhen == RemoveNever
		then ifM (expectedPresent dest key)
			( stop
			, go True (pure $ Right False)
			)
		else go False (Remote.hasKey dest key)
  where
	go fastcheck isthere = do
		showMoveAction removewhen key ai
		next $ toPerform dest removewhen key afile fastcheck =<< isthere

expectedPresent :: Remote -> Key -> Annex Bool
expectedPresent dest key = do
	remotes <- Remote.keyPossibilities key
	return $ dest `elem` remotes

toPerform :: Remote -> RemoveWhen -> Key -> AssociatedFile -> Bool -> Either String Bool -> CommandPerform
toPerform dest removewhen key afile fastcheck isthere =
	case isthere of
		Left err -> do
			showNote err
			stop
		Right False -> do
			showAction $ "to " ++ Remote.name dest
			ok <- notifyTransfer Upload afile $
				upload (Remote.uuid dest) key afile stdRetry $
					Remote.storeKey dest key afile
			if ok
				then finish False $
					Remote.logStatus dest key InfoPresent
				else do
					when fastcheck $
						warning "This could have failed because --fast is enabled."
					stop
		Right True -> finish True $
			unlessM (expectedPresent dest key) $
				Remote.logStatus dest key InfoPresent
  where
	finish deststartedwithcopy setpresentremote = case removewhen of
		RemoveNever -> do
			setpresentremote
			next $ return True
		RemoveSafe -> lockContentForRemoval key $ \contentlock -> do
			srcuuid <- getUUID
			let destuuid = Remote.uuid dest
			willDropMakeItWorse srcuuid destuuid deststartedwithcopy key afile >>= \case
				DropAllowed -> drophere setpresentremote contentlock "moved"
				DropCheckNumCopies -> do
					numcopies <- getAssociatedFileNumCopies afile
					(tocheck, verified) <- verifiableCopies key [srcuuid]
					verifyEnoughCopiesToDrop "" key (Just contentlock)
						 numcopies [srcuuid] verified
						 (UnVerifiedRemote dest : tocheck)
						 (drophere setpresentremote contentlock . showproof)
						 (faileddrophere setpresentremote)
				DropWorse -> faileddrophere setpresentremote
	showproof proof = "proof: " ++ show proof
	drophere setpresentremote contentlock reason = do
		liftIO $ debugM "move" $ unwords
			[ "Dropping from here"
			, "(" ++ reason ++ ")"
			]
		-- Drop content before updating location logs,
		-- in case disk space is very low this frees
		-- up space before writing data to disk.
		removeAnnex contentlock
		next $ do
			() <- setpresentremote
			Command.Drop.cleanupLocal key
	faileddrophere setpresentremote = do
		showLongNote "(Use --force to override this check, or adjust numcopies.)"
		showLongNote "Content not dropped from here."
		next $ do
			() <- setpresentremote
			return False

fromStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> Remote -> CommandStart
fromStart removewhen afile key ai src = case removewhen of
	RemoveNever -> stopUnless (not <$> inAnnex key) go
	RemoveSafe -> go
  where
	go = stopUnless (fromOk src key) $ do
		showMoveAction removewhen key ai
		next $ fromPerform src removewhen key afile

fromOk :: Remote -> Key -> Annex Bool
fromOk src key 
	| Remote.hasKeyCheap src =
		either (const checklog) return =<< haskey
	| otherwise = checklog
  where
	haskey = Remote.hasKey src key
	checklog = do
		u <- getUUID
		remotes <- Remote.keyPossibilities key
		return $ u /= Remote.uuid src && elem src remotes

fromPerform :: Remote -> RemoveWhen -> Key -> AssociatedFile -> CommandPerform
fromPerform src removewhen key afile = do
	showAction $ "from " ++ Remote.name src
	ifM (inAnnex key)
		( dispatch removewhen True True
		, dispatch removewhen False =<< go
		)
  where
	go = notifyTransfer Download afile $ 
		download (Remote.uuid src) key afile stdRetry $ \p ->
			getViaTmp (Remote.retrievalSecurityPolicy src) (RemoteVerify src) key $ \t ->
				Remote.retrieveKeyFile src key afile t p
	dispatch _ _ False = stop -- failed
	dispatch RemoveNever _ True = next $ return True -- copy complete
	dispatch RemoveSafe deststartedwithcopy True = lockContentShared key $ \_lck -> do
		let srcuuid = Remote.uuid src
		destuuid <- getUUID
		willDropMakeItWorse srcuuid destuuid deststartedwithcopy key afile >>= \case
			DropAllowed -> dropremote "moved"
			DropCheckNumCopies -> do
				numcopies <- getAssociatedFileNumCopies afile
				(tocheck, verified) <- verifiableCopies key [Remote.uuid src]
				verifyEnoughCopiesToDrop "" key Nothing numcopies [Remote.uuid src] verified
					tocheck (dropremote . showproof) faileddropremote
			DropWorse -> faileddropremote		
	showproof proof = "proof: " ++ show proof
	dropremote reason = do
		liftIO $ debugM "move" $ unwords
			[ "Dropping from remote"
			, show src
			, "(" ++ reason ++ ")"
			]
		ok <- Remote.removeKey src key
		next $ Command.Drop.cleanupRemote key src ok
	faileddropremote = do
		showLongNote "(Use --force to override this check, or adjust numcopies.)"
		showLongNote $ "Content not dropped from " ++ Remote.name src ++ "."
		next $ return False

{- Moves (or copies) the content of an annexed file from reachable remotes
 - to the current repository.
 -
 - When moving, the content is removed from all the reachable remotes that
 - it can safely be removed from. -}
toHereStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> CommandStart
toHereStart removewhen afile key ai = case removewhen of
	RemoveNever -> stopUnless (not <$> inAnnex key) go
	RemoveSafe -> go
  where
	go = do
		rs <- Remote.keyPossibilities key
		forM_ rs $ \r ->
			includeCommandAction $ do
				showMoveAction removewhen key ai
				next $ fromPerform r removewhen key afile
		stop

{- The goal of this command is to allow the user maximum freedom to move
 - files as they like, while avoiding making bad situations any worse
 - than they already were.
 -
 - When the destination repository already had a copy of a file
 - before the move operation began, dropping it from the source
 - repository reduces the number of copies, and should fail if
 - that would violate numcopies settings.
 -
 - On the other hand, when the destiation repository does not already
 - have a copy of a file, it can be dropped without making numcopies
 - worse, so the move is allowed even if numcopies is not met.
 -
 - Similarly, a file can move from an untrusted repository to another
 - untrusted repository, even if that is the only copy of the file.
 -
 - But, moving a file from a repository with higher trust to an untrusted
 - repository must still check that there are enough other copies to be
 - safe.
 -
 - Also, required content settings should not be violated.
 -
 - This function checks all that. It needs to know if the destination
 - repository already had a copy of the file before the move began.
 -}
willDropMakeItWorse :: UUID -> UUID -> Bool -> Key -> AssociatedFile -> Annex DropCheck
willDropMakeItWorse srcuuid destuuid deststartedwithcopy key afile =
	ifM (Command.Drop.checkRequiredContent srcuuid key afile)
		( if deststartedwithcopy
			then unlessforced DropCheckNumCopies
			else ifM checktrustlevel
				( return DropAllowed
				, unlessforced DropCheckNumCopies
				)
		, unlessforced DropWorse
		)
  where
	unlessforced r = ifM (Annex.getState Annex.force)
		( return DropAllowed
		, return r
		)
	checktrustlevel = do
		desttrust <- lookupTrust destuuid
		srctrust <- lookupTrust srcuuid
		return (desttrust > UnTrusted || desttrust >= srctrust)

data DropCheck = DropWorse | DropAllowed | DropCheckNumCopies
