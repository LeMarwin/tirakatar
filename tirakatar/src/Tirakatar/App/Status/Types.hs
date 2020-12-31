module Tirakatar.App.Status.Types(
    StatusUpdate(..)
  ) where

import Data.Time
import Tirakatar.Text
import Tirakatar.App.Language

data StatusUpdate = NotActive
  deriving (Show, Eq, Ord)

instance LocalizedPrint StatusUpdate where
  localizedShow l v = case l of
    English -> case v of
      NotActive -> "Not active"
    Russian -> case v of
      NotActive -> "Отключена"
