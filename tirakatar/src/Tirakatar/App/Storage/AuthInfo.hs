module Tirakatar.App.Storage.AuthInfo (
    initAuthInfo
  , loadAuthInfo
  ) where

import Control.Monad.Except

import Tirakatar.Crypto
import Tirakatar.App.Language
import Tirakatar.App.Monad
import Tirakatar.App.Native
import Tirakatar.App.Platform
import Tirakatar.App.Storage.Util
import Tirakatar.App.Types
import Tirakatar.App.Localization.AuthInfo
import Tirakatar.Types

import qualified Data.Text as T

initAuthInfo :: (MonadIO m, PlatformNatives, HasStoreDir m)
  => Mnemonic
  -> StorageName
  -> Password
  -> Bool
  -> m (Either AuthInfoAlert AuthInfo)
initAuthInfo mnemonic login pass isPass = do
  mstorage <- createStorage mnemonic (login, pass)
  case mstorage of
    Left err -> do
      logWrite $ localizedShow English err
      pure $ Left $ CreateStorageAlert err
    Right s -> case passwordToECIESPrvKey pass of
      Left _ -> pure $ Left GenerateECIESKeyAlert
      Right k -> do
        let fname = "meta_wallet_" <> (T.replace " " "_" login)
        when (isAndroid && isPass) $ storeValue fname True True
        pure $ Right AuthInfo {
          _authInfo'storage = s
        , _authInfo'eciesPubKey = toPublic k
        , _authInfo'login = login
        , _authInfo'isUpdate = False
        , _authInfo'isPlain = pass == ""
        }

loadAuthInfo :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => StorageName
  -> Password
  -> m (Either AuthInfoAlert (AuthInfo, Password))
loadAuthInfo login pass = do
  mstorage <- loadStorageFromFile login pass
  case mstorage of
    Left err -> pure $ Left $ LoadStorageAlert err
    Right s -> case passwordToECIESPrvKey pass of
      Left _ -> pure $ Left GenerateECIESKeyAlert
      Right k -> pure $ Right (
          AuthInfo {
            _authInfo'storage = s
          , _authInfo'eciesPubKey = toPublic k
          , _authInfo'login = login
          , _authInfo'isUpdate = False
          , _authInfo'isPlain = pass == ""
          }
        , pass
        )
