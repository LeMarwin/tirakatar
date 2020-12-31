module Tirakatar.App.Monad.Client (
    MonadIndexClient(..)
  -- * Reexports
  , SockAddr
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Function (on)
import Data.Functor.Misc (Const2(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time(NominalDiffTime, getCurrentTime, diffUTCTime)
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef

import Tirakatar.App.Monad.Async
import Tirakatar.App.Monad.Prim
import Tirakatar.App.Settings
import Tirakatar.App.Util

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ===========================================================================
--    Monad Client. Implements all required things for client operations
-- ===========================================================================

class MonadBaseConstr t m => MonadIndexClient t m | m -> t where
  -- | Get active addrs ref
  getActiveAddrsRef :: m ()
