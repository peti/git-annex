{- git-annex v4 -> v5 upgrade support
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V4 where

import Annex.Common

{- Direct mode only upgrade. v4 to v5 indirect update is a no-op.
 -
 - Since direct mode is removed, this upgrade no longer does anything. -}
upgrade :: Bool -> Annex Bool
upgrade _ = return True
