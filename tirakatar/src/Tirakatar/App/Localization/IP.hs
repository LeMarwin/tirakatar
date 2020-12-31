module Tirakatar.App.Localization.IP
  (
    IPStrings(..)
  ) where

import Tirakatar.App.Language

data IPStrings =
  IPParseFailed
  deriving (Eq, Show)

instance LocalizedPrint IPStrings where
  localizedShow l v = case l of
    English -> case v of
      IPParseFailed          -> "Failed to parse IP address"
    Russian -> case v of
      IPParseFailed          -> "IP адрес неверного формата"
