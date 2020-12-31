module Tirakatar.App.Camera(
    openCamara
  , getResultCamara
  , waiterResultCamera
  , debugCameraPage
  ) where

import Data.Text (Text)
import Data.Text as T
import Tirakatar.App.Elements
import Tirakatar.App.Monad
import Tirakatar.App.Native

openCamara :: MonadFrontBase t m => Event t () -> m (Event t ())
openCamara e = runOnUiThread $ ffor e $ \_ -> do
  cameraWork ""
  pure ()

getResultCamara :: MonadFrontBase t m => Event t () -> m (Event t Text)
getResultCamara e = runOnUiThread $ ffor e $ const cameraGetResult

waiterResultCamera :: MonadFrontBase t m => Event t () -> m (Event t Text)
waiterResultCamera startE = mdo
  resE <- getResultCamara $ leftmost [startE, nextE]
  nextE <- delay 1.0 $ fforMaybe resE $ \v ->
    if T.null v then Just () else Nothing
  pure $ fforMaybe resE $ \v -> 
    if T.null v then Nothing else Just v

debugCameraPage :: MonadFrontBase t m => m ()
debugCameraPage = do
  cameraE <- outlineButton ("Debug QR scan"::Text)
  openE <- openCamara cameraE
  openGoE <- delay 1.0 openE
  resE <- waiterResultCamera openGoE
  resD <- holdDyn "RESULT" resE
  h4 $ dynText resD
  pure ()
