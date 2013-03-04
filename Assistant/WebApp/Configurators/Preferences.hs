{- git-annex assistant general preferences
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Preferences (
	getPreferencesR
) where

import Assistant.WebApp.Common
import Assistant.WebApp.Html
import qualified Annex
import qualified Git
import Config
import Locations.UserConfig
import Utility.DataUnits

import qualified Data.Text as T
import System.Log.Logger

data PrefsForm = PrefsForm
	{ diskReserve :: Text
	, numCopies :: Int
	, autoStart :: Bool
	, debugEnabled :: Bool
	}

prefsAForm :: PrefsForm -> AForm WebApp WebApp PrefsForm
prefsAForm def = PrefsForm
	<$> areq (storageField `withNote` diskreservenote)
		"Disk reserve" (Just $ diskReserve def)
	<*> areq (positiveIntField `withNote` numcopiesnote)
		"Number of copies" (Just $ numCopies def)
	<*> areq (checkBoxField `withNote` autostartnote)
		"Auto start" (Just $ autoStart def)
	<*> areq (checkBoxField `withNote` debugnote)
		"Enable debug logging" (Just $ debugEnabled def)
  where
	diskreservenote = whtml $ do
		hbr
		hspan "Avoid downloading files from other repositories when there is too little free disk space."
	numcopiesnote = whtml $ do
		hbr
		hspan "Only drop a file after verifying that other repositories contain this many copies."
	debugnote = toWidget $ \render ->
		hspan $ "View Log" ==> render LogR []
	autostartnote = whtml $
		hspan "Start the git-annex assistant at boot or on login."

	positiveIntField = check isPositive intField
	  where
		isPositive i
			| i > 0 = Right i
			| otherwise = Left notPositive
		notPositive :: Text
		notPositive = "This should be 1 or more!"

	storageField = check validStorage textField
	  where
		validStorage t
			| T.null t = Right t
			| otherwise =  case readSize dataUnits $ T.unpack t of
				Nothing -> Left badParse
				Just _ -> Right t
		badParse :: Text
		badParse = "Parse error. Expected something like \"100 megabytes\" or \"2 gb\""

getPrefs :: Annex PrefsForm
getPrefs = PrefsForm
	<$> (T.pack . roughSize storageUnits False . annexDiskReserve <$> Annex.getGitConfig)
	<*> (annexNumCopies <$> Annex.getGitConfig)
	<*> inAutoStartFile
	<*> ((==) <$> (pure $ Just DEBUG) <*> (liftIO $ getLevel <$> getRootLogger))

storePrefs :: PrefsForm -> Annex ()
storePrefs p = do
	setConfig (annexConfig "diskreserve") (T.unpack $ diskReserve p)
	setConfig (annexConfig "numcopies") (show $ numCopies p)
	unlessM ((==) <$> pure (autoStart p) <*> inAutoStartFile) $ do
		here <- fromRepo Git.repoPath
		liftIO $ if autoStart p
			then addAutoStartFile here
			else removeAutoStartFile here
	liftIO $ updateGlobalLogger rootLoggerName $ setLevel $
		if debugEnabled p then DEBUG else WARNING

getPreferencesR :: Handler RepHtml
getPreferencesR = page title (Just Configuration) $ do
	((result, form), enctype) <- lift $ do
		current <- runAnnex undefined getPrefs
		runFormGet $ renderBootstrap $ prefsAForm current
	case result of
		FormSuccess new -> lift $ do
			runAnnex undefined $ storePrefs new
			redirect ConfigurationR
		_ -> stdForm enctype form buttons -- TODO add heroUnit title
  where
	title = "Preferences"
	buttons = toWidget $ \render ->
		"Save Preferences" <>|<> render ConfigurationR []

inAutoStartFile :: Annex Bool
inAutoStartFile = do
	here <- fromRepo Git.repoPath
	any (`equalFilePath` here) <$> liftIO readAutoStartFile
