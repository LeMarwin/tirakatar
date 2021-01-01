module Tirakatar.Types.Restore(
    AccountSource(..)
  ) where

import GHC.Generics

data AccountSource =
    AccountGenerated -- ^ Account was generated and we don't need to scan all history
  | AccountRestored -- ^ Account was restored from seed and we need to scan all history
  deriving (Show, Eq, Generic)
