{-# OPTIONS_GHC -Wno-type-defaults #-}
module Tirakatar.App.Storage.Util(
    listStorages
  , getLastStorage
  , setLastStorage
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteArray           (Bytes, convert)
import Data.ByteArray.Sized     (SizedByteArray, unsafeSizedByteArray)
import Data.ByteString          (ByteString)
import Data.List                (foldl')
import Data.Maybe
import Data.Proxy
import Data.Text                (Text)
import Data.Text.Encoding
import Data.SafeCopy
import Data.Serialize

import Tirakatar.Crypto
import Tirakatar.Text
import Tirakatar.Types
import Tirakatar.App.Localization.Native
import Tirakatar.App.Localization.Storage
import Tirakatar.App.Platform

import qualified Data.ByteString as BS
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

storageFilePrefix :: Text
storageFilePrefix = "storage_"

storageBackupFilePrefix :: Text
storageBackupFilePrefix = "backup_"

-- | Scan storage folder for all accounts
listStorages :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => m [StorageName]
listStorages = do
  ns <- listKeys
  pure $ catMaybes . fmap isWallet $ ns
  where
    isWallet n = let
      (a, _) = T.breakOn storageFilePrefix n
      in if T.null a then Just (T.drop (T.length storageFilePrefix) n) else Nothing

-- | Name of file where last storage name is stored
lastStorageFile :: Text
lastStorageFile = ".last-storage"

-- | Try to check `.last-storage` file to get name of storages
getLastStorage :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => m (Maybe StorageName)
getLastStorage = do
  logWrite $ "Reading last storage file " <> lastStorageFile
  mres <- retrieveValue lastStorageFile Nothing
  pure $ either (const Nothing) id $ mres

-- | Try to write `.last-storage` file to set name of storage
setLastStorage :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => Maybe StorageName -> m ()
setLastStorage mname = do
  logWrite $ "Writing last storage file " <> lastStorageFile
  storeValue lastStorageFile mname False
