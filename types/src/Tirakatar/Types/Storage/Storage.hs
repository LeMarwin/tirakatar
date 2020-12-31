-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
module Tirakatar.Types.Storage.Storage
  (
    TirStorage(..)
  , EncryptedTirStorage(..)
  -- * Export lenses
  , storage'encryptedPrvStorage
  , storage'pubStorage
  , storage'name
  , encryptedStorage'ciphertext
  , encryptedStorage'salt
  , encryptedStorage'iv
  , encryptedStorage'eciesPoint
  , encryptedStorage'authTag
  ) where

import Control.Lens
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.ECC (Curve_X25519, Point, decodePoint)
import Crypto.Error
import Data.ByteArray
import Data.ByteString (ByteString)
import Data.Proxy
import Data.SafeCopy
import Data.Serialize
import Data.Text (Text,)

import Tirakatar.Types.Storage.Public
import Tirakatar.Types.Storage.Private

-- ====================================================================
--      TirStorage. Not encrypted
-- ====================================================================

data TirStorage = TirStorage {
    _storage'encryptedPrvStorage :: !EncryptedPrvStorage
  , _storage'pubStorage          :: !PubStorage
  , _storage'name                :: !Text
  }

instance SafeCopy TirStorage where
  version = 1
  putCopy (TirStorage e p w) = contain $ do
    safePut e >> safePut p >> put w
  getCopy = contain $ TirStorage <$> safeGet <*> safeGet <*> get

-- ====================================================================
--      EncryptedTirStorage
-- ====================================================================

data EncryptedTirStorage = EncryptedTirStorage {
    _encryptedStorage'ciphertext :: !ByteString
  , _encryptedStorage'salt       :: !ByteString
  , _encryptedStorage'iv         :: !(IV AES256)
  , _encryptedStorage'eciesPoint :: !(Point Curve_X25519)
  , _encryptedStorage'authTag    :: !AuthTag
  }

instance SafeCopy EncryptedTirStorage where
  version = 1
  putCopy EncryptedTirStorage{..} = contain $ do
    safePut _encryptedStorage'ciphertext
    safePut _encryptedStorage'salt
    safePut _encryptedStorage'iv
    safePut (convert _encryptedStorage'eciesPoint :: ByteString)
    safePut (convert _encryptedStorage'authTag :: ByteString)
  getCopy = contain $ do
    cip <- safeGet
    salt <- safeGet
    iv <- safeGet
    eciesbs :: ByteString <- safeGet
    tagbs :: ByteString <- safeGet
    let authTag = AuthTag (convert tagbs :: Bytes)
        curve = Proxy :: Proxy Curve_X25519
    case decodePoint curve eciesbs of
      CryptoFailed _ -> fail "failed to read eciesPoint"
      CryptoPassed eciesPoint -> pure $ EncryptedTirStorage cip salt iv eciesPoint authTag

-- ====================================================================
-- These instances are required only for the current version
-- ====================================================================
makeLenses ''TirStorage
makeLenses ''EncryptedTirStorage

instance Eq TirStorage where
  a == b = _storage'name a == _storage'name b
