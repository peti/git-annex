{- Types for Storer and Retriever actions for remotes.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.StoreRetrieve where

import Annex.Common
import Utility.Metered

import qualified Data.ByteString.Lazy as L

-- A source of a Key's content.
data ContentSource
	= FileContent FilePath
	| ByteContent L.ByteString

isByteContent :: ContentSource -> Bool
isByteContent (ByteContent _) = True
isByteContent (FileContent _) = False

-- Action that stores a Key's content on a remote.
-- Can throw exceptions.
type Storer = Key -> ContentSource -> MeterUpdate -> Annex ()

-- Action that retrieves a Key's content from a remote, passing it to a
-- callback, which will fully consume the content before returning.
-- Throws exception if key is not present, or remote is not accessible.
type Retriever = Key -> MeterUpdate -> (ContentSource -> Annex ()) -> Annex ()

-- Action that removes a Key's content from a remote.
-- Succeeds if key is already not present.
-- Throws an exception if the remote is not accessible.
type Remover = Key -> Annex ()

-- Checks if a Key's content is present on a remote.
-- Throws an exception if the remote is not accessible.
type CheckPresent = Key -> Annex Bool
