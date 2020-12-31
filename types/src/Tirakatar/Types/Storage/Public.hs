-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
-- Read the README.md to learn now to work with migrations
module Tirakatar.Types.Storage.Public
  (
    PubStorage(..)
  -- * Export lenses
  , pubStorage'rootPubKey
  ) where

import Control.Lens
import Data.SafeCopy
import Data.Serialize

import Tirakatar.Types.Keys.Prim

data PubStorage = PubStorage {
    _pubStorage'rootPubKey          :: !TirRootXPubKey
  } deriving (Eq, Show, Read)

instance SafeCopy PubStorage where
  version = 1
  putCopy PubStorage{..} = contain $ do
    put _pubStorage'rootPubKey
  getCopy = contain $ PubStorage <$> get

-- This instances is required only for the current version
makeLenses ''PubStorage
