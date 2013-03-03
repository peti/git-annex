{- git-annex assistant general preferences
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Preferences where

import Assistant.WebApp.Common
import Config

data PrefsForm = PrefsForm
	{ debug

getPreferencesR :: Handler RepHtml
getPreferencesR = page "Preferences" (Just Configuration) $ do
	((result, form), enctype) <- lift $ 
		current <- getPrefs
		runFormGet $ renderBootstrap $ prefsForm current
	case result of
		FormSuccess new -> do
			storePrefs new
			redirect ConfigurationR
		_ -> $(widgetFile "configurators/preferences")
