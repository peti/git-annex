{- git-annex repository versioning
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Annex.Version where

import Annex.Common
import Config
import Git.Types
import Types.RepoVersion
import qualified Annex

import qualified Data.Map as M

defaultVersion :: RepoVersion
defaultVersion = RepoVersion 8

latestVersion :: RepoVersion
latestVersion = RepoVersion 8

supportedVersions :: [RepoVersion]
supportedVersions = map RepoVersion [8]

upgradableVersions :: [RepoVersion]
#ifndef mingw32_HOST_OS
upgradableVersions = map RepoVersion [0..7]
#else
upgradableVersions = map RepoVersion [2..7]
#endif

autoUpgradeableVersions :: M.Map RepoVersion RepoVersion
autoUpgradeableVersions = M.fromList
	[ (RepoVersion 3, latestVersion)
	, (RepoVersion 4, latestVersion)
	, (RepoVersion 5, latestVersion)
	, (RepoVersion 6, latestVersion)
	, (RepoVersion 7, latestVersion)
	]

versionField :: ConfigKey
versionField = annexConfig "version"

getVersion :: Annex (Maybe RepoVersion)
getVersion = annexVersion <$> Annex.getGitConfig

setVersion :: RepoVersion -> Annex ()
setVersion (RepoVersion v) = setConfig versionField (show v)

removeVersion :: Annex ()
removeVersion = unsetConfig versionField
