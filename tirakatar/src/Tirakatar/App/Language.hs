{-# OPTIONS_GHC -Wno-orphans #-}
-- | Module that fullfils data family required by `reflex-localize`
module Tirakatar.App.Language(
    Language(..)
  , allLanguages
  , module Reflex.Localize
  , module Reflex.Localize.Dom
  ) where

import Tirakatar.Aeson
import GHC.Generics (Generic)
import Reflex.Localize
import Reflex.Localize.Dom
import Reflex.Localize.Language

-- | Languages that are supported by the app
data instance Language
  = English
  | Russian
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance LocalizedPrint Language where
  localizedShow _ v = case v of
    English -> "English"
    Russian -> "Русский"

$(deriveJSON aesonOptions 'English)

allLanguages :: [Language]
allLanguages = [minBound .. maxBound]
