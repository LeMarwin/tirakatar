-- Primary stuff, that is required for all versions of Keys package
module Tirakatar.Types.Keys.Prim
  (
    RootSigPrvKey(..)
  , RootSigPubKey(..)
  , RootEncPrvKey(..)
  , RootEncPubKey(..)
  ) where

import Data.ByteString        (ByteString)
import Data.Serialize         (Serialize, get, put)
import Data.ByteArray (convert)
import Tirakatar.Crypto.ECIES
import Tirakatar.Crypto.ECDSA

newtype RootEncPrvKey = RootEncPrvKey {unRootEncPrvKey :: ECIESPrvKey}
  deriving (Show)
newtype RootEncPubKey = RootEncPubKey {unRootEncPubKey :: ECIESPubKey}
  deriving (Show)

newtype RootSigPrvKey = RootSigPrvKey {unRootSigPrvKey :: ECDSAPrvKey}
  deriving (Show)
newtype RootSigPubKey = RootSigPubKey {unRootSigPubKey :: ECDSAPubKey}
  deriving (Show)

instance Serialize RootEncPrvKey where
  put (RootEncPrvKey k) = put $ convert @_ @ByteString k
  get = do
    bs <- get @ByteString
    case secretKey bs of
      CryptoFailed err -> fail $ show err
      CryptoPassed k -> pure $ RootEncPrvKey k

instance Serialize RootEncPubKey where
  put (RootEncPubKey k) = put $ convert @_ @ByteString k
  get = do
    bs <- get @ByteString
    case publicKey bs of
      CryptoFailed err -> fail $ show err
      CryptoPassed k -> pure $ RootEncPubKey k

instance Serialize RootSigPrvKey where
  put (RootSigPrvKey k) = put $ convert @_ @ByteString k
  get = do
    bs <- get @ByteString
    case ecdsaPrivKey bs of
      CryptoFailed err -> fail $ show err
      CryptoPassed k -> pure $ RootSigPrvKey k

instance Serialize RootSigPubKey where
  put (RootSigPubKey k) = put $ convert @_ @ByteString k
  get = do
    bs <- get @ByteString
    case ecdsaPubKey bs of
      CryptoFailed err -> fail $ show err
      CryptoPassed k -> pure $ RootSigPubKey k
