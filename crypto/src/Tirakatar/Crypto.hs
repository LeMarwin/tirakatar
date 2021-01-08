module Tirakatar.Crypto (
    module X
  , defaultEntropyLength
  , getEntropy
  ) where

import Tirakatar.Crypto.AES256    as X
import Tirakatar.Crypto.ECDSA     as X
import Tirakatar.Crypto.ECIES     as X
import Tirakatar.Crypto.Mnemonic  as X
import Tirakatar.Crypto.PBKDF     as X
import Tirakatar.Crypto.Util      as X
import Tirakatar.Crypto.Base58    as X

import qualified System.Entropy   as E

-- | According to the BIP32 the allowed size of entropy is between 16 and 64 bytes (32 bytes is advised).
-- The mnemonic must encode entropy in a multiple of 4 bytes.
-- With 32 bytes of entropy generated mnemonic will contain 24 words.
defaultEntropyLength :: Int
defaultEntropyLength = 64

getEntropy :: IO Entropy
getEntropy = E.getEntropy defaultEntropyLength
