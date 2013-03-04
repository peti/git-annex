{- WebApp html generation
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Assistant.WebApp.Html (
	module X,
	stdForm
) where

import Assistant.WebApp.Types
import Utility.Html as X
import Common

import Yesod (getYesod, Enctype, toWidget, lift)
import Text.Blaze.Html5 (Html, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

{- A standard form, including the auth token. -}
stdForm :: Enctype -> Widget -> Html -> Widget
stdForm enctype form buttons = do
	toWidget "<form class=\"form-horizontal\" enctype=\""
	toWidget $ toHtml enctype
	toWidget "\"><fieldset>"
	toWiget form
	token <- toValue . secretToken <$> lift getYesod
	whtml $ do
		hiddenInput ! A.name "auth" ! A.value token
		buttons
	toWidget "</fieldset></form>"
