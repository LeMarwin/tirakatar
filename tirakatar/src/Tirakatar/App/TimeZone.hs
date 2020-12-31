module Tirakatar.App.TimeZone(
    getGetTimeZone
  ) where

import Data.Time (TimeZone)
import Tirakatar.App.Monad
import Tirakatar.App.Native

getGetTimeZone :: MonadFrontBase t m => Event t () -> m (Event t TimeZone)
getGetTimeZone e = runOnUiThread $ ffor e $ const getTimeZone
