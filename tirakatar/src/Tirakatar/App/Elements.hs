{-# LANGUAGE OverloadedLists #-}
module Tirakatar.App.Elements(
    container
  , row
  , column
  , column10
  , column20
  , column25
  , column33
  , column40
  , column50
  , column60
  , column67
  , column75
  , column80
  , column90
  , column100
  , br
  , spanEl
  , spanClass
  , divClass'
  , elClassDyn
  , divClassDyn
  , spanClassDyn
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , li
  , ul
  , par
  , parClass
  , bold
  , form
  , formClass
  , fieldset
  , fieldsetClass
  , label
  , imgClass
  , linedText
  , colonize
  , colonize_
  , buttonClass
  , buttonClassDynLabel
  , outlineButton
  , clearButton
  , divButton
  , spanButton
  , outlineTextIconButton
  , outlineTextIconButtonTypeButton
  , outlineTextIconButtonClass
  , outlineSubmitTextIconButtonClass
  , outlineIconButtonClass
  , hyperlink
  , badge
  -- * JS functions
  , selElementFocus
  , module Tirakatar.App.Util
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Language.Javascript.JSaddle hiding ((!!))
import Reflex.Localize

import Tirakatar.App.Monad.Front
import Tirakatar.App.Native
import Tirakatar.App.OpenUrl
import Tirakatar.App.Util

import qualified Data.Text as T

container :: DomBuilder t m => m a -> m a
container = divClass "container"

row :: DomBuilder t m => m a -> m a
row = divClass "row"

column :: DomBuilder t m => m a -> m a
column = divClass "column"

column10, column20, column25, column33, column40, column50, column60, column67, column75, column80, column90, column100 :: DomBuilder t m => m a -> m a
column10 = divClass "column column-10"
column20 = divClass "column column-20"
column25 = divClass "column column-25"
column33 = divClass "column column-33"
column40 = divClass "column column-40"
column50 = divClass "column column-50"
column60 = divClass "column column-60"
column67 = divClass "column column-67"
column75 = divClass "column column-75"
column80 = divClass "column column-80"
column90 = divClass "column column-90"
column100 = divClass "column column-100"

br :: DomBuilder t m => m ()
br = el "br" blank

spanEl :: DomBuilder t m => m a -> m a
spanEl = el "span"

spanClass :: DomBuilder t m => Text -> m a -> m a
spanClass = elClass "span"

elClassDyn :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m a -> m a
elClassDyn eln classD = elDynAttr eln $ do
  v <- classD
  pure [("class", v)]

divClass' :: (DomBuilder t m, PostBuild t m) => Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
divClass' = elClass' "div"

divClassDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
divClassDyn = elClassDyn "div"

spanClassDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
spanClassDyn = elClassDyn "span"

h1 :: DomBuilder t m => m a -> m a
h1 = el "h1"

h2 :: DomBuilder t m => m a -> m a
h2 = el "h2"

h3 :: DomBuilder t m => m a -> m a
h3 = el "h3"

h4 :: DomBuilder t m => m a -> m a
h4 = el "h4"

h5 :: DomBuilder t m => m a -> m a
h5 = el "h5"

h6 :: DomBuilder t m => m a -> m a
h6 = el "h6"

li :: DomBuilder t m => m a -> m a
li = el "li"

ul :: DomBuilder t m => m a -> m a
ul = el "ul"

par :: DomBuilder t m => m a -> m a
par = el "p"

parClass :: DomBuilder t m => Text -> m a -> m a
parClass = elClass "p"

bold :: DomBuilder t m => m a -> m a
bold = el "b"

form :: DomBuilder t m => m a -> m a
form = elAttr "form" [("onsubmit", "return false;")]

formClass :: DomBuilder t m => Text -> m a -> m a
formClass classVal = elAttr "form" [("onsubmit", "return false;"), ("class", classVal)]

fieldset :: DomBuilder t m => m a -> m a
fieldset = el "fieldset"

fieldsetClass :: DomBuilder t m => Text -> m a -> m a
fieldsetClass classVal = elAttr "fieldset" [("class", classVal)]

label :: DomBuilder t m => Text -> m a -> m a
label i = elAttr "label" ("for" =: i)

imgClass :: DomBuilder t m => Text -> Text -> m ()
imgClass src classVal = elAttr "img" [
    ("src", src)
  , ("class", classVal)] $ pure ()

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked n xs = take n xs : chunked n (drop n xs)

-- | Places each line of text in seperate `dynText` widget. Caution: for fast changing
-- dynamics it can cause redraw flickering (e.x. triggering resize each time).
linedText :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t Text -> m ()
linedText textD = void $ simpleList (T.lines <$> textD) (\t -> dynText t >> br)

-- | Traverse container and render widgets for each element in rows
colonize :: (DomBuilder t m)
  => Int -- ^ Amount of columns in row
  -> [a] -- ^ Collection of data
  -> (a -> m b) -- ^ Widget
  -> m [b] -- ^ Results
colonize n as w = fmap concat $ traverse (row . traverse (column . w)) $ chunked n as

-- | Traverse container and render widgets for each element in rows
colonize_ :: (DomBuilder t m)
  => Int -- ^ Amount of columns in row
  -> [a] -- ^ Collection of data
  -> (a -> m b) -- ^ Widget
  -> m () -- ^ Results
colonize_ n as w = traverse_ (row . traverse_ (column . w)) $ chunked n as

-- | Button with CSS classes
mkButton :: (DomBuilder t m, PostBuild t m) => Text -> Map Text Text -> Dynamic t Text -> m a -> m (Event t a)
mkButton eltp attrs classValD ma = do
  let classesD = do
        classVal <- classValD
        pure $ attrs <> [("class", classVal)]
  (e, a) <- elDynAttr' eltp classesD ma
  return $ a <$ domEvent Click e

-- | Button with CSS classes
buttonClass :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> m (Event t ())
buttonClass classValD lbl = mkButton "button" [("onclick", "return false;")] classValD . dynText =<< localized lbl

-- | Button with CSS classes
buttonClassDynLabel :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> Dynamic t lbl -> m (Event t ())
buttonClassDynLabel classValD lblD = do
  langD <- getLanguage
  mkButton "button" [("onclick", "return false;")] classValD . dynText $ do
    l <- langD
    lbl <- lblD
    pure $ localizedShow l lbl

-- | Bright button with dark outline
outlineButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => lbl -> m (Event t ())
outlineButton = buttonClass "button button-outline"

-- | Button without border
clearButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => lbl -> m (Event t ())
clearButton = buttonClass "button button-clear"

 -- | Span that acts like a button with CSS classes
spanButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> m (Event t ())
spanButton classValD lbl = mkButton "span" [] classValD . dynText =<< localized lbl

-- | Div that acts like a button with CSS classes
divButton :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m (Event t a)
divButton = mkButton "div" []

-- outlineButton with icon from Font Awesome library
-- The first parameter is the button text
-- The second parameter is the icon class
-- Usage example:
-- >>> outlineTextIconButton CSPaste "fas fa-clipboard"
-- As a result, such an element will be created:
-- <button class="button button-outline href="return false;">
--   Scan QR code
--   <span class="button-icon-wrapper">
--     <i class="fas fa-qrcode"></i>
--   </span>
-- </button>
outlineTextIconButtonClass :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> Text -> m (Event t ())
outlineTextIconButtonClass classValD lbl i =
  mkButton "button" [("onclick", "return false;")] (("button button-outline " <>) <$> classValD) $ do
    dynText =<< localized lbl
    elClass "span" "button-icon-wrapper" $ elClass "i" i blank

outlineTextIconButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => lbl -> Text -> m (Event t ())
outlineTextIconButton lbl i =
  mkButton "button" [("onclick", "return false;")] "button button-outline" $ do
    dynText =<< localized lbl
    elClass "span" "button-icon-wrapper" $ elClass "i" i blank

outlineTextIconButtonTypeButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => lbl -> Text -> m (Event t ())
outlineTextIconButtonTypeButton lbl i =
  mkButton "button" [("onclick", "return false;"), ("type", "button")] "button button-outline" $ do
    dynText =<< localized lbl
    elClass "span" "button-icon-wrapper" $ elClass "i" i blank

outlineIconButtonClass :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> Text -> m (Event t ())
outlineIconButtonClass classValD i =
  mkButton "button" [("onclick", "return false;")] (("button button-outline " <>) <$> classValD) $ elClass "i" i blank

outlineSubmitTextIconButtonClass :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> Text -> m (Event t ())
outlineSubmitTextIconButtonClass classValD lbl i =
  mkButton "button" [("onclick", "return false;"), ("type", "submit")] (("button button-outline " <>) <$> classValD) $ do
    dynText =<< localized lbl
    elClass "span" "button-icon-wrapper" $ elClass "i" i blank

-- | Link with custom click handler which opens link in external browser
hyperlink :: (MonadFrontBase t m, PlatformNatives, MonadLocalized t m)
  => Dynamic t Text -> Text -> Text -> m ()
hyperlink classValD lbl url = do
  clickeE <- spanButton classValD lbl
  _ <- openOpenUrl $ url <$ clickeE
  pure ()

badge :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl) => Text -> lbl -> m ()
badge classes lbl = elClass "code" classes $ dynText =<< localized lbl

selElementFocus :: MonadJSM m => RawElement GhcjsDomSpace -> m ()
selElementFocus rawEl = liftJSM $ do
  void $ eval func
  void $ jsg1 funcName (toJSVal rawEl)
  where
    (funcName :: Text) = "tirakatar_set_el_focus"
    (func :: Text) = " tirakatar_set_el_focus = function(el) {el.focus();}"
