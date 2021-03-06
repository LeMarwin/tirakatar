-- |
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Tirakatar.Types.Orphanage where

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Data.Aeson
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.SafeCopy
import Data.Serialize
import Data.Time
import Data.Text (Text)
import Data.Vector (Vector)

import Tirakatar.Text

import qualified Data.ByteString.Short as BSS
import qualified Data.Vector as V
import qualified Data.Text as T

instance FromJSON ShortByteString where
  parseJSON = withText "ShortByteString" $
    either (fail "Failed to parse a ShortByteString") (pure . BSS.toShort) . hex2bsTE
  {-# INLINE parseJSON #-}

instance ToJSON ShortByteString where
  toJSON = String . bs2Hex . BSS.fromShort
  {-# INLINE toJSON #-}

instance SafeCopy (IV AES256) where
  putCopy iv = contain $ safePut (convert iv :: ByteString)
  getCopy = contain $ do
    iv :: ByteString <- safeGet
    maybe (fail "failed to make iv") pure $ makeIV iv

instance Serialize a => Serialize (Vector a) where
  get = do
    n <- get
    V.replicateM n get
  put v = do
    put $ length v
    mapM_ put v

instance Serialize UTCTime where
  put (UTCTime (ModifiedJulianDay d) t) = do
    put d
    put $ diffTimeToPicoseconds t
  get = do
    d <- get
    t <- get
    pure $ UTCTime (ModifiedJulianDay d) $ picosecondsToDiffTime t

instance Serialize Text where
  put = put . T.unpack
  get = fmap T.pack $ get
