module Tirakatar.Server.Monad.Prim
  (
    HasShutdownFlag(..)
  ) where

import Control.Concurrent.STM

class HasShutdownFlag m where
  getShutdownFlag  :: m (TVar Bool)
