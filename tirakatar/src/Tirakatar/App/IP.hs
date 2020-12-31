{-# OPTIONS_GHC -Wno-orphans #-}
module Tirakatar.App.IP(
    IP
  , SockAddr
  , parseIP
  , toSockAddr
  , makeSockAddr
  ) where

import Data.IP (IP, toSockAddr)
import Data.Text (Text, unpack)
import Tirakatar.App.Elements.Input.Class
import Network.Socket (SockAddr)
import Text.Read (readMaybe)
import Tirakatar.Text

import Tirakatar.App.Localization.IP

-- | Parsing IPv4 and IPv6 addresses
parseIP :: Text -> Maybe IP
parseIP = readMaybe . unpack

-- | Parsing IPv4 and IPv6 addresses and makes socket address from them
makeSockAddr :: IP -> Int -> SockAddr
makeSockAddr ip pnum = toSockAddr (ip, fromIntegral pnum)

instance (LocalizedPrint l, Wrappable IPStrings l) => Inputable l IP where
  displayInput _ = showt
  parseInput = maybe (Left $ wrap IPParseFailed) Right . parseIP
