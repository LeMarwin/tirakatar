module Tirakatar.App.Localization.Input
  (
    InputStrings(..)
  ) where

import Tirakatar.App.Language

data InputStrings =
    IntParseError
  | WordParseError
  deriving (Eq, Show)

instance LocalizedPrint InputStrings where
  localizedShow l v = case l of
    English -> case v of
      IntParseError          -> "Failed to parse integer"
      WordParseError         -> "Failed to parse natural number"
    Russian -> case v of
      IntParseError          -> "Не удалось прочитать целое число"
      WordParseError         -> "Число должно быть целочисленным или 0"
