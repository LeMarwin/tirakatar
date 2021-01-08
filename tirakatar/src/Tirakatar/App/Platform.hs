{-# LANGUAGE CPP #-}
module Tirakatar.App.Platform(
    Platform(..)
  , currentPlatform
  , isDesktop
  , isAndroid
  , isTestnet
  ) where

import GHC.Generics (Generic)
import qualified Data.Vector as V

-- | Platform the app is compiled for.
data Platform = DesktopLinux | Android
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Get current platform of the app
currentPlatform :: Platform
#ifdef ANDROID
currentPlatform = Android
#else
currentPlatform = DesktopLinux
#endif

-- | Helpers to test current platform
isDesktop, isAndroid :: Bool
isDesktop = currentPlatform == DesktopLinux
isAndroid = currentPlatform == Android

-- | Global flag that indicates that we need to compile for testnet.
-- The value of the function is controlled by `testnet` cabal flag.
isTestnet :: Bool
#ifdef TESTNET
isTestnet = True
#else
isTestnet = False
#endif
