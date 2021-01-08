module Tirakatar.Crypto.ECIES(
    Curve_X25519
  , Point
  , Scalar
  , ECIESPrvKey
  , ECIESPubKey
  , CryptoFailable(..)
  , deriveEncrypt
  , deriveDecrypt
  , decodeEciesPubKey
  , encodeEciesPubKey
  , secretKey
  , publicKey
  , dhSecret
  , toPublic
  ) where

import Crypto.ECC (Curve_X25519, Point, Scalar, encodePoint, decodePoint)
import Crypto.Error
import Crypto.PubKey.Curve25519 (secretKey, publicKey, dhSecret, toPublic)
import Crypto.PubKey.ECIES
import Data.Proxy
import Data.ByteArray

type ECIESPrvKey = Scalar Curve_X25519

type ECIESPubKey = Point Curve_X25519

decodeEciesPubKey :: ByteArray bs => bs -> CryptoFailable ECIESPubKey
decodeEciesPubKey = decodePoint (Proxy :: Proxy Curve_X25519)

encodeEciesPubKey :: ByteArray bs => ECIESPubKey -> bs
encodeEciesPubKey = encodePoint (Proxy :: Proxy Curve_X25519)
