module Tirakatar.App.Log.Event(
    logEvent
  , logEventWith
  ) where

import Data.Text (Text)
import Tirakatar.Text
import Tirakatar.App.Monad.Prim
import Tirakatar.App.Native
import Reflex

logEvent :: (MonadBaseConstr t m, PlatformNatives, Show a) => Text -> Event t a -> m (Event t a)
logEvent t e = performEvent $ ffor e $ \v -> do
  logWrite $ t <> showt v
  pure v

logEventWith :: (MonadBaseConstr t m, PlatformNatives) => (a -> Text) -> Event t a -> m (Event t a)
logEventWith f e = performEvent $ ffor e $ \v -> do
  logWrite $ f v
  pure v