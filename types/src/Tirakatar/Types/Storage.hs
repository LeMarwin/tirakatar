module Tirakatar.Types.Storage
  (
    StorageName
  , Password
  , module Tirakatar.Types.Storage.Private
  , module Tirakatar.Types.Storage.Public
  , module Tirakatar.Types.Storage.Storage
  ) where

import Data.Text

import Tirakatar.Types.Storage.Private
import Tirakatar.Types.Storage.Public
import Tirakatar.Types.Storage.Storage

type StorageName = Text

type Password = Text
