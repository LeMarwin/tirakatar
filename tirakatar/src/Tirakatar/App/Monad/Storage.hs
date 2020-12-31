module Tirakatar.App.Monad.Storage
  (
    MonadStorage(..)
  , HasPubStorage(..)
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Text (Text)
import Network.Haskoin.Block (Timestamp)
import Reflex

import Tirakatar.App.Monad.Prim
import Tirakatar.App.Native
import Tirakatar.App.Types
import Tirakatar.Crypto
import Tirakatar.Types

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Block as HB
import qualified Network.Haskoin.Transaction as HT

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getEncryptedPrvStorage :: m EncryptedPrvStorage
  getStorageName         :: m Text
  getPubStorage          :: m PubStorage
  getPubStorageD         :: m (Dynamic t PubStorage)
  storeStorage           :: Text -> Event t () -> m (Event t ())
  modifyPubStorage       :: Text -> Event t (PubStorage -> Maybe PubStorage) -> m (Event t ())
  -- | Get mutex that guards writing or reading from storage file
  getStoreMutex          :: m (MVar ())
  -- | Channel that writes down given storage to disk in separate thread. First element in tuple is tracing info (caller).
  getStoreChan           :: m (TChan (Text, AuthInfo))

class MonadIO m => HasPubStorage m where
  askPubStorage :: m PubStorage

instance MonadIO m => HasPubStorage (ReaderT PubStorage m) where
  askPubStorage = ask
