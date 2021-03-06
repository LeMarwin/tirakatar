module Tirakatar.App.Clipboard(
    clipboardCopy
  , clipboardPaste
  , clipboardDebug
  , copyButton
  ) where

import Data.Text (Text)
import Tirakatar.App.Elements
import Tirakatar.App.Language
import Tirakatar.App.Monad
import Tirakatar.App.Native

-- | Clipboard control widget that allows to put text into clipboard. Returns
-- event when the text is copied with the same text.
clipboardCopy :: MonadFrontBase t m => Event t Text -> m (Event t Text)
clipboardCopy e = runOnUiThread $ ffor e $ \str -> do
  copyStr str
  pure str

-- | Clipboard control widget that allows to copy text from clipboard.
clipboardPaste :: MonadFrontBase t m => Event t () -> m (Event t Text)
clipboardPaste e = runOnUiThread $ ffor e $ const pasteStr

data CopyStr = StrCopy | StrPaste

instance LocalizedPrint CopyStr where
  localizedShow l v = case l of
    English -> case v of
      StrCopy -> "Copy"
      StrPaste -> "Paste"
    Russian -> case v of
      StrCopy -> "Копировать"
      StrPaste -> "Вставить"

-- | Debug widgete for clipboard
clipboardDebug :: MonadFrontBase t m => m ()
clipboardDebug = mdo
  tinput <- inputElement $ def & inputElementConfig_setValue .~ pastedE
  copyE <- buttonClass (pure "form__btn confirm-button") StrCopy
  pasteE <- buttonClass (pure "form__btn confirm-button") StrPaste
  let copyStrE = tag (current $ _inputElement_value tinput) copyE
  _ <- clipboardCopy copyStrE
  pastedE <- clipboardPaste pasteE
  pure ()

-- | Helper to make copying button
copyButton :: MonadFrontBase t m => Dynamic t Text -> m (Event t Text)
copyButton textD = do
  butE <- buttonClass (pure "form__btn confirm-button") StrCopy
  clipboardCopy $ current textD `tag` butE
