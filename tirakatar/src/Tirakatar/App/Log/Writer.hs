{-# LANGUAGE CPP #-}
module Tirakatar.App.Log.Writer(
    logWriter
  ) where

import Control.Monad
import Data.Aeson
import Data.Text.Encoding
import Tirakatar.App.Log.Types
import Tirakatar.App.Monad.Front
import Tirakatar.App.Native

import qualified Data.ByteString.Lazy as BS

#ifdef ANDROID
import Tirakatar.App.Android.Native()
#else
import Tirakatar.App.Desktop.Native()
#endif

-- | Widget that writes down to internal storage all log entries
logWriter :: MonadFrontBase t m => Event t LogEntry -> m ()
logWriter e = performEvent_ $ ffor e $ \entry -> do
  size <- either (const 0) id <$> getStoreFileSize logStorageKey
  when (size >= logStorageMaxSize) $ void $ moveStoredFile logStorageKey logStorageKeyOld
  appendStoredFile logStorageKey $ (<> "\n") . decodeUtf8 . BS.toStrict . encode $ entry
