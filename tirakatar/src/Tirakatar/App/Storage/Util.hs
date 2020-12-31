{-# OPTIONS_GHC -Wno-type-defaults #-}
module Tirakatar.App.Storage.Util(
    storageFilePrefix
  , storageBackupFilePrefix
  , saveStorageToFile
  , saveStorageSafelyToFile
  , loadStorageFromFile
  , listStorages
  , lastStorageFile
  , getLastStorage
  , setLastStorage
  , createStorage
  , createPrvStorage
  , createPubStorage
  , encryptPrvStorage
  , decryptPrvStorage
  , encryptStorage
  , passwordToECIESPrvKey
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

saveStorageToFile :: (MonadIO m, MonadRandom m, HasStoreDir m, PlatformNatives)
  => Text -> ECIESPubKey -> TirStorage -> m ()
saveStorageToFile caller pubKey storage = do
  let fname = storageFilePrefix <> T.replace " " "_" (_storage'name storage)
      backupFname = storageBackupFilePrefix <> fname
  logWrite $ "[" <> caller <> "]: Storing to " <> fname
  encryptedStorage <- encryptStorage storage pubKey
  case encryptedStorage of
    Left _ -> fail "Failed to encrypt storage"
    Right encStorage -> do
      void $ moveStoredFile fname backupFname
      let bs = runPut $ safePut encStorage
      storeBS fname bs True

-- | The same as saveStorageToFile, but does not fail and returns the error instead
saveStorageSafelyToFile :: (MonadIO m, MonadRandom m, HasStoreDir m, PlatformNatives)
  => Text -> ECIESPubKey -> TirStorage -> m (Either StorageAlert ())
saveStorageSafelyToFile caller pubKey storage = do
  let fname = storageFilePrefix <> T.replace " " "_" (_storage'name storage)
      backupFname = storageBackupFilePrefix <> fname
  logWrite $ "[" <> caller <> "]: Storing to " <> fname
  encryptedStorage <- encryptStorage storage pubKey
  case encryptedStorage of
    Left err -> pure $ Left err
    Right encStorage -> do
      void $ moveStoredFile fname backupFname
      let bs = runPut $ safePut encStorage
      fmap Right $ storeBS fname bs True

loadStorageFromFile :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => StorageName -> Password -> m (Either StorageAlert TirStorage)
loadStorageFromFile login pass = do
  let fname = storageFilePrefix <> T.replace " " "_" login
      backupFname = fname <> storageBackupFilePrefix
  storageResp <- retrieveBS fname
  case storageResp of
    Left err -> pure $ Left $ SANativeAlert err
    Right storageBs -> case runGet safeGet storageBs of
      Left _ -> do
        logWrite $ "Failed to decode wallet from: " <> fname <> "\nReading from backup: " <> backupFname
        backupStorageResp <- retrieveBS fname
        case backupStorageResp of
            Left err -> pure $ Left $ SANativeAlert err
            Right backupStorageBs -> case runGet safeGet backupStorageBs of
              Left err -> pure $ Left $ SADecodeError $ T.pack err
              Right backupStorage -> pure $ passwordToECIESPrvKey pass >>= decryptStorage backupStorage
      Right storage -> pure $ passwordToECIESPrvKey pass >>= decryptStorage storage

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

createStorage :: MonadIO m
  => Mnemonic -- ^ Mnemonic to generate keys
  -> (StorageName, Password) -- ^ Wallet file name and encryption password
  -> m (Either StorageAlert TirStorage)
createStorage mnemonic (login, pass) = case mnemonicToSeed "" mnemonic of
   Left err -> pure $ Left $ SAMnemonicFail $ showt err
   Right seed -> do
    let rootPrvKey = TirRootXPrvKey $ makeXPrvKey seed
        prvStorage = createPrvStorage mnemonic rootPrvKey
        pubStorage = createPubStorage rootPrvKey
    encryptPrvStorageResult <- encryptPrvStorage prvStorage pass
    case encryptPrvStorageResult of
      Left err -> pure $ Left err
      Right encryptedPrvStorage -> pure $ Right $ TirStorage encryptedPrvStorage pubStorage login

createPrvStorage :: Mnemonic -> TirRootXPrvKey -> PrvStorage
createPrvStorage mnemonic rootPrvKey = PrvStorage mnemonic rootPrvKey

createPubStorage :: TirRootXPrvKey -> PubStorage
createPubStorage rootPrvKey = PubStorage rootPubKey
  where rootPubKey = TirRootXPubKey $ deriveXPubKey $ unTirRootXPrvKey rootPrvKey

encryptPrvStorage :: MonadIO m => PrvStorage -> Password -> m (Either StorageAlert EncryptedPrvStorage)
encryptPrvStorage prvStorage password = liftIO $ do
  salt :: ByteString <- genRandomSalt
  let secKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
  iv <- genRandomIV (undefined :: AES256)
  case iv of
    Nothing -> pure $ Left $ SACryptoError "Failed to generate an AES initialization vector"
    Just iv' -> do
      let prvStorageBS = runPut $ safePut prvStorage
      case encrypt secKey iv' prvStorageBS of
        Left err -> pure $ Left $ SACryptoError $ showt err
        Right ciphertext -> pure $ Right $ EncryptedPrvStorage ciphertext salt iv'

decryptPrvStorage :: EncryptedPrvStorage -> Password -> Either StorageAlert PrvStorage
decryptPrvStorage encryptedPrvStorage password =
  case decrypt secKey iv ciphertext of
    Left err -> Left $ SACryptoError $ showt err
    Right decryptedPrvStorage -> do
      let decodedPrvStorage = runGet safeGet decryptedPrvStorage
      case decodedPrvStorage of
        Left err -> Left $ SADecryptError $ showt err
        Right dps -> Right dps
  where
    salt = _encryptedPrvStorage'salt encryptedPrvStorage
    secKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
    iv = _encryptedPrvStorage'iv encryptedPrvStorage
    ciphertext = _encryptedPrvStorage'ciphertext encryptedPrvStorage

encryptStorage :: (MonadIO m, MonadRandom m) => TirStorage -> ECIESPubKey -> m (Either StorageAlert EncryptedTirStorage)
encryptStorage storage pubKey = do
  let curve = Proxy :: Proxy Curve_X25519
  deriveEncryptResult <- deriveEncrypt curve pubKey
  case deriveEncryptResult of
    CryptoFailed err -> pure $ Left $ SACryptoError $ showt err
    CryptoPassed (eciesPoint, sharedSecret) -> do
      salt :: ByteString <- genRandomSalt
      let secKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params sharedSecret salt) :: Key AES256 ByteString
      iv' <- genRandomIV (undefined :: AES256)
      case iv' of
        Nothing -> pure $ Left $ SACryptoError "Failed to generate an initialization vector"
        Just iv -> do
          let storageBS = runPut $ safePut storage
              ivBS = convert iv :: ByteString
              eciesPointBS = encodePoint curve eciesPoint :: ByteString
              encryptedData = encryptWithAEAD AEAD_GCM secKey iv (BS.concat [salt, ivBS, eciesPointBS]) storageBS defaultAuthTagLength
          case encryptedData of
            Left err -> pure $ Left $ SACryptoError $ showt err
            Right (authTag, ciphertext) -> pure $ Right $ EncryptedTirStorage ciphertext salt iv eciesPoint authTag


decryptStorage :: EncryptedTirStorage -> ECIESPrvKey -> Either StorageAlert TirStorage
decryptStorage encryptedStorage prvKey = do
  let curve = Proxy :: Proxy Curve_X25519
      ciphertext = _encryptedStorage'ciphertext encryptedStorage
      salt       = _encryptedStorage'salt       encryptedStorage
      iv         = _encryptedStorage'iv         encryptedStorage
      eciesPoint = _encryptedStorage'eciesPoint encryptedStorage
      authTag    = _encryptedStorage'authTag    encryptedStorage
  case deriveDecrypt curve eciesPoint prvKey of
      CryptoFailed err -> Left $ SACryptoError $ showt err
      CryptoPassed sharedSecret -> do
        let ivBS = convert iv :: ByteString
            eciesPointBS = encodePoint curve eciesPoint :: ByteString
            secKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params sharedSecret salt) :: Key AES256 ByteString
            decryptedData = decryptWithAEAD AEAD_GCM secKey iv (BS.concat [salt, ivBS, eciesPointBS]) ciphertext authTag
        case decryptedData of
          Nothing -> Left $ SADecryptError "Failed to decrypt storage"
          Just decryptedStorage -> case storage of
            Left err -> Left $ SACryptoError $ showt err
            Right s -> Right s
            where
              storage = runGet safeGet decryptedStorage

passwordToECIESPrvKey :: Password -> Either StorageAlert ECIESPrvKey
passwordToECIESPrvKey password = case secretKey passwordHash of
  CryptoFailed _ -> Left $ SACryptoError "Failed to generate an ECIES secret key from password"
  CryptoPassed key -> Right key
  where
    passwordHash = fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) BS.empty :: ByteString
