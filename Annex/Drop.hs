{- dropping of unwanted content
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Drop where

import Annex.Common
import qualified Annex
import Logs.Trust
import Annex.NumCopies
import Types.Remote (uuid)
import qualified Remote
import qualified Command.Drop
import Command
import Annex.Wanted
import qualified Database.Keys
import Git.FilePath

import qualified Data.Set as S
import System.Log.Logger (debugM)

type Reason = String

{- Drop a key from local and/or remote when allowed by the preferred content
 - and numcopies settings.
 -
 - The UUIDs are ones where the content is believed to be present.
 - The Remote list can include other remotes that do not have the content;
 - only ones that match the UUIDs will be dropped from.
 -
 - If allowed to drop fromhere, that drop will be done last. This is done
 - because local drops do not need any LockedCopy evidence, and so dropping
 - from local last allows the content to be removed from more remotes.
 -
 - A VerifiedCopy can be provided as an optimisation when eg, a key
 - has just been uploaded to a remote.
 -
 - In direct mode, all associated files are checked, and only if all
 - of them are unwanted are they dropped.
 -
 - The runner is used to run commands, and so can be either callCommand
 - or commandAction.
 -}
handleDropsFrom :: [UUID] -> [Remote] -> Reason -> Bool -> Key -> AssociatedFile -> [VerifiedCopy] -> (CommandStart -> CommandCleanup) -> Annex ()
handleDropsFrom locs rs reason fromhere key afile preverified runner = do
	l <- do
		g <- Annex.gitRepo
		map (`fromTopFilePath` g) <$> Database.Keys.getAssociatedFiles key
	let fs = case afile of
		Just f -> nub (f : l)
		Nothing -> l
	n <- getcopies fs
	void $ if fromhere && checkcopies n Nothing
		then go fs rs n >>= dropl fs
		else go fs rs n
  where
	getcopies fs = do
		(untrusted, have) <- trustPartition UnTrusted locs
		numcopies <- if null fs
			then getNumCopies
			else maximum <$> mapM getFileNumCopies fs
		return (NumCopies (length have), numcopies, S.fromList untrusted)

	{- Check that we have enough copies still to drop the content.
	 - When the remote being dropped from is untrusted, it was not
	 - counted as a copy, so having only numcopies suffices. Otherwise,
	 - we need more than numcopies to safely drop. -}
	checkcopies (have, numcopies, _untrusted) Nothing = have > numcopies
	checkcopies (have, numcopies, untrusted) (Just u)
		| S.member u untrusted = have >= numcopies
		| otherwise = have > numcopies
	
	decrcopies (have, numcopies, untrusted) Nothing =
		(NumCopies (fromNumCopies have - 1), numcopies, untrusted)
	decrcopies v@(_have, _numcopies, untrusted) (Just u)
		| S.member u untrusted = v
		| otherwise = decrcopies v Nothing

	go _ [] n = pure n
	go fs (r:rest) n
		| uuid r `S.notMember` slocs = go fs rest n
		| checkcopies n (Just $ Remote.uuid r) =
			dropr fs r n >>= go fs rest
		| otherwise = pure n

	checkdrop fs n u a
		| null fs = check $ -- no associated files; unused content
			wantDrop True u (Just key) Nothing
		| otherwise = check $
			allM (wantDrop True u (Just key) . Just) fs
		where
			check c = ifM c
				( dodrop n u a
				, return n
				)

	dodrop n@(have, numcopies, _untrusted) u a = 
		ifM (safely $ runner $ a numcopies)
			( do
				liftIO $ debugM "drop" $ unwords
					[ "dropped"
					, fromMaybe (key2file key) afile
					, "(from " ++ maybe "here" show u ++ ")"
					, "(copies now " ++ show (fromNumCopies have - 1) ++ ")"
					, ": " ++ reason
					]
				return $ decrcopies n u
			, return n
			)

	dropl fs n = checkdrop fs n Nothing $ \numcopies ->
		Command.Drop.startLocal afile numcopies key preverified

	dropr fs r n  = checkdrop fs n (Just $ Remote.uuid r) $ \numcopies ->
		Command.Drop.startRemote afile numcopies key r

	slocs = S.fromList locs
	
	safely a = either (const False) id <$> tryNonAsync a

