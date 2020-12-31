-- Primary stuff, that is required for all versions of Keys package
module Tirakatar.Types.Keys.Prim
  (
    KeyPurpose(..)
  , TirRootXPrvKey(..)
  , TirXPrvKey(..)
  , TirRootXPubKey(..)
  , TirXPubKey(..)
  , getXPrvKey
  , putXPrvKey
  , getXPubKey
  , putXPubKey
  , xPrvExport
  , xPubExport
  , xPrvImport
  , xPubImport
  ) where

import Control.Monad
import Data.Serialize         (Serialize, get, put)
import Data.Serialize.Get     (Get, getWord32be, getWord8, runGet)
import Data.Serialize.Put     (Putter, putWord32be, putWord8, runPut)
import Data.Text              (Text, pack, unpack)
import GHC.Generics

import Tirakatar.Aeson
import Tirakatar.Crypto.Keys
import Tirakatar.Crypto.Util
import Tirakatar.Types.Orphanage()

-- | Supported key purposes. It represents /change/ field in BIP44 derivation path.
-- External chain is used for addresses that are meant to be visible outside of the wallet (e.g. for receiving payments).
-- Internal chain is used for addresses which are not meant to be visible outside of the wallet and is used for return transaction change.
data KeyPurpose = External | Internal
  deriving (Eq, Ord, Show, Read, Generic, Serialize)
$(deriveJSON defaultOptions ''KeyPurpose)

-- ====================================================================
--      Private keys: TirRootXPrvKey, TirXPrvKey
-- ====================================================================

-- | Wrapper for a root extended private key (a key without assigned network)
newtype TirRootXPrvKey = TirRootXPrvKey {unTirRootXPrvKey :: XPrvKey}
  deriving (Eq, Show, Read)

instance Serialize TirRootXPrvKey where
  get = fmap TirRootXPrvKey $ XPrvKey
    <$> getWord8
    <*> getWord32be
    <*> getWord32be
    <*> get
    <*> getPadPrvKey

  put (TirRootXPrvKey k) = do
    putWord8     $ xPrvDepth k
    putWord32be  $ xPrvParent k
    putWord32be  $ xPrvIndex k
    put          $ xPrvChain k
    putPadPrvKey $ xPrvKey k

-- | Wrapper around XPrvKey for easy to/from json manipulations
data TirXPrvKey = TirXPrvKey { unTirXPrvKey :: !XPrvKey}
  deriving (Eq, Show, Read)

instance Serialize TirXPrvKey where
  get = TirXPrvKey <$> getXPrvKey
  put (TirXPrvKey k) = putXPrvKey k

-- ====================================================================
--      Public keys: TirRootXPubKey, TirXPubKey
-- ====================================================================

-- | Wrapper for a root extended public key (a key without assigned network)
newtype TirRootXPubKey = TirRootXPubKey {unTirRootXPubKey :: XPubKey}
  deriving (Eq, Show, Read)

instance Serialize TirRootXPubKey where
  get = fmap TirRootXPubKey $ XPubKey
    <$> getWord8
    <*> getWord32be
    <*> getWord32be
    <*> get
    <*> (pubKeyPoint <$> get)
  put (TirRootXPubKey k) = do
    putWord8    $ xPubDepth k
    putWord32be $ xPubParent k
    putWord32be $ xPubIndex k
    put         $ xPubChain k
    put         $ wrapPubKey True (xPubKey k)

-- | Wrapper around XPubKey for easy to/from json manipulations
data TirXPubKey =
    TirXPubKey {
      tirXPubKey   :: !XPubKey
    , tirXPubLabel :: !Text
    }
  deriving (Eq, Show, Read)

instance Serialize TirXPubKey where
  get = do
    k <- getXPubKey
    l <- fmap pack get
    pure $ TirXPubKey k l
  put (TirXPubKey k l) = do
    putXPubKey k
    put $ unpack l

instance Ord TirXPubKey where
  compare (TirXPubKey k1 _) (TirXPubKey k2 _) = compare (xPubExport k1) (xPubExport k2)

-- ====================================================================
--      Getters and putters for base crypto keys
-- ====================================================================

-- | Parse a binary extended private key.
getXPrvKey :: Get XPrvKey
getXPrvKey = XPrvKey
  <$> getWord8
  <*> getWord32be
  <*> getWord32be
  <*> get
  <*> getPadPrvKey

-- | Serialize an extended private key.
putXPrvKey :: Putter XPrvKey
putXPrvKey k = do
  putWord8     $ xPrvDepth k
  putWord32be  $ xPrvParent k
  putWord32be  $ xPrvIndex k
  put          $ xPrvChain k
  putPadPrvKey $ xPrvKey k

-- | Parse a binary extended public key.
getXPubKey :: Get XPubKey
getXPubKey = XPubKey
  <$> getWord8
  <*> getWord32be
  <*> getWord32be
  <*> get
  <*> (pubKeyPoint <$> get)

-- | Serialize an extended public key.
putXPubKey :: Putter XPubKey
putXPubKey k = do
  putWord8    $ xPubDepth k
  putWord32be $ xPubParent k
  putWord32be $ xPubIndex k
  put         $ xPubChain k
  put         $ wrapPubKey True (xPubKey k)

-- | Exports an extended private key to the BIP32 key export format ('Base58').
xPrvExport :: XPrvKey -> Base58
xPrvExport = encodeBase58Check . runPut . putXPrvKey

-- | Exports an extended public key to the BIP32 key export format ('Base58').
xPubExport :: XPubKey -> Base58
xPubExport = encodeBase58Check . runPut . putXPubKey

-- | Decodes a BIP32 encoded extended private key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPrvImport :: Base58 -> Maybe XPrvKey
xPrvImport = eitherToMaybe . runGet getXPrvKey <=< decodeBase58Check

-- | Decodes a BIP32 encoded extended public key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPubImport :: Base58 -> Maybe XPubKey
xPubImport = eitherToMaybe . runGet getXPubKey <=< decodeBase58Check
