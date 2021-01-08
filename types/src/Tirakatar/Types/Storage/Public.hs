-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
-- Read the README.md to learn now to work with migrations
module Tirakatar.Types.Storage.Public
  (
    PubStorage(..)
  -- * Export lenses
  , pubStorage'encPubKey
  , pubStorage'sigPubKey
  ) where

import Control.Lens
import Data.SafeCopy
import Data.Serialize

import Tirakatar.Types.Keys.Prim

data PubStorage = PubStorage {
    _pubStorage'encPubKey :: !RootEncPubKey
  , _pubStorage'sigPubKey :: !RootSigPubKey
  } deriving (Show)

instance SafeCopy PubStorage where
  version = 1
  putCopy PubStorage{..} = contain $ do
    put _pubStorage'encPubKey
    put _pubStorage'sigPubKey
  getCopy = contain $ PubStorage <$> get <*> get

-- This instances is required only for the current version
makeLenses ''PubStorage
