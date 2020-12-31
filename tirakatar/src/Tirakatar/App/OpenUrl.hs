module Tirakatar.App.OpenUrl(
    openOpenUrl
  ) where

import Data.Text (Text)
import Tirakatar.App.Monad
import Tirakatar.App.Native

openOpenUrl :: MonadFrontBase t m => Event t Text -> m (Event t ())
openOpenUrl = runOnUiThread . fmap openUrl
{-# INLINE openOpenUrl #-}
