module Tirakatar.Crypto.ECDSA
  (
    ECDSAPrvKey
  , ECDSAPubKey
  , Signature
  -- * Smart constructors
  , ecdsaPrivKey
  , ecdsaPubKey
  , signature
  -- * Size constants
  , ecdsaPublicKeySize
  , ecdsaSecretKeySize
  , ecdsaSignatureSize
  -- * Methods
  , ecdsaToPublic
  , sign
  , verify
  , ecdsaGenPrivate
  ) where

import Crypto.Error
import Crypto.PubKey.Ed25519
import Crypto.Random.Types
import Data.ByteArray

type ECDSAPrvKey = SecretKey
type ECDSAPubKey = PublicKey

ecdsaPubKey :: ByteArrayAccess ba => ba -> CryptoFailable ECDSAPubKey
ecdsaPubKey = publicKey
ecdsaPrivKey :: ByteArrayAccess ba => ba -> CryptoFailable ECDSAPrvKey
ecdsaPrivKey = secretKey
ecdsaToPublic :: ECDSAPrvKey -> ECDSAPubKey
ecdsaToPublic = toPublic
ecdsaGenPrivate :: MonadRandom m => m ECDSAPrvKey
ecdsaGenPrivate = generateSecretKey

ecdsaPublicKeySize :: Int
ecdsaPublicKeySize = publicKeySize
ecdsaSecretKeySize :: Int
ecdsaSecretKeySize = secretKeySize
ecdsaSignatureSize :: Int
ecdsaSignatureSize = signatureSize
