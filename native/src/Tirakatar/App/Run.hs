-- | The module overrides standart reflex-dom widget to get access to low level
-- Android callbacks.
module Tirakatar.App.Run(
    PlatformRun(..)
  ) where

import Tirakatar.App.Run.Callbacks
import Language.Javascript.JSaddle.Types

class PlatformRun where
  run :: (RunCallbacks -> JSM ()) -> IO ()
