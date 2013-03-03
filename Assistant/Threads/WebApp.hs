{- git-annex assistant webapp thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Assistant.Threads.WebApp where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.DashBoard
import Assistant.WebApp.SideBar
import Assistant.WebApp.Notifications
import Assistant.WebApp.Configurators
import Assistant.WebApp.Configurators.Edit
import Assistant.WebApp.Configurators.Local
import Assistant.WebApp.Configurators.Ssh
import Assistant.WebApp.Configurators.Pairing
import Assistant.WebApp.Configurators.AWS
import Assistant.WebApp.Configurators.WebDAV
import Assistant.WebApp.Configurators.XMPP
import Assistant.WebApp.Configurators.Preferences
import Assistant.WebApp.Documentation
import Assistant.WebApp.Control
import Assistant.WebApp.OtherRepos
import Assistant.Types.ThreadedMonad
import Utility.WebApp
import Utility.TempFile
import Utility.FileMode
import Git

import Yesod
import Yesod.Static
import Network.Socket (SockAddr)
import Data.Text (pack, unpack)

mkYesodDispatch "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

type Url = String

webAppThread
	:: AssistantData
	-> UrlRenderer
	-> Bool
	-> Maybe (IO String)
	-> Maybe (Url -> FilePath -> IO ())
	-> NamedThread
webAppThread assistantdata urlrenderer noannex postfirstrun onstartup = thread $ liftIO $ do
	webapp <- WebApp
		<$> pure assistantdata
		<*> (pack <$> genRandomToken)
		<*> getreldir
		<*> pure $(embed "static")
		<*> newWebAppState
		<*> pure postfirstrun
		<*> pure noannex
	setUrlRenderer urlrenderer $ yesodRender webapp (pack "")
	app <- toWaiAppPlain webapp
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp app' $ \addr -> if noannex
		then withTempFile "webapp.html" $ \tmpfile _ ->
			go addr webapp tmpfile Nothing
		else do
			let st = threadState assistantdata
			htmlshim <- runThreadState st $ fromRepo gitAnnexHtmlShim
			urlfile <- runThreadState st $ fromRepo gitAnnexUrlFile
			go addr webapp htmlshim (Just urlfile)
  where
	thread = namedThread "WebApp"
	getreldir
		| noannex = return Nothing
		| otherwise = Just <$>
			(relHome =<< absPath
				=<< runThreadState (threadState assistantdata) (fromRepo repoPath))
	go addr webapp htmlshim urlfile = do
		let url = myUrl webapp addr
		maybe noop (`writeFileProtected` url) urlfile
		writeHtmlShim "Starting webapp..." url htmlshim
		maybe noop (\a -> a url htmlshim) onstartup

myUrl :: WebApp -> SockAddr -> Url
myUrl webapp addr = unpack $ yesodRender webapp urlbase HomeR []
  where
	urlbase = pack $ "http://" ++ show addr
