{- Html generation for Yesod, without the Hamlet template Haskell.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Utility.Html where

import Yesod (toWidget, GWidget)
import Text.Blaze.Html5 (Html, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

{- A simple Widget from pure html. -}
whtml :: Html -> GWidget sub master ()
whtml = toWidget . const

{- Makes a link out of a yesod route. Example use:
 -
 - toWidget $ \render ->
 - 	"click here" ==> render SomeR
 -}
(==>) :: forall href. H.ToValue href => Html -> href -> Html
link ==> href = H.a link ! A.href (toValue href)

{- Convenience exports of Blaze HTML combinators, avoiding names
 - like 'a' that conflict with commonly used variable names etc. -}
hbr :: Html
hbr = H.br
hspan :: Html -> Html
hspan = H.span

hiddenInput :: Html
hiddenInput = H.input ! A.type_ "hidden"

{- Bootstrap hero unit, with a title. -}
heroUnit :: Html -> Html -> Html
heroUnit title body = H.div ! A.class_ "span9" ! A.class_ "hero-unit" $ do
	H.h2 title
	H.p body

{- Primary submit button. -}
submitButton :: Html -> Html
submitButton = H.button ! A.class_ "btn" ! A.class_ "btn-primary" ! A.type_ "submit"

{- Cancel button -}
cancelButton :: forall href. H.ToValue href => href -> Html
cancelButton href = "Cancel" ==> href ! A.class_ "btn"

{- Submit and Cancel buttons.
 - Supply a label for the submit button and a href for the cancel button. -}
(<>|<>) :: forall href. H.ToValue href => Html -> href -> Html
label <>|<> href = H.div ! A.class_ "form-actions" $ do
	submitButton label
	cancelButton href
