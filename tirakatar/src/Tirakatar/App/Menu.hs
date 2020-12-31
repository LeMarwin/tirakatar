module Tirakatar.App.Menu(
    headerWidgetDesktop
  , headerWidgetAndroid
  , headerWidgetOnlyBackBtn
  ) where

import Data.Text (Text)
import Tirakatar.App.Elements
import Tirakatar.App.Language
import Tirakatar.App.Monad

headerWidgetOnlyBackBtn :: MonadFrontBase t m => m ()
headerWidgetOnlyBackBtn = divClass "header" $ do
  stD <- getRetractStack
  void $ backButton "header-button" $ null <$> stD

headerWidgetDesktop :: MonadFront t m => Dynamic t Text -> Maybe (Dynamic t (m ())) -> m ()
headerWidgetDesktop titleD thisWidget = divClass "header-wrapper" $ do
  menuIsHiddenD <- headerDesktop titleD
  menuDesktop menuIsHiddenD thisWidget

headerWidgetAndroid :: MonadFront t m => Dynamic t Text -> Maybe (Dynamic t (m ())) -> m ()
headerWidgetAndroid titleD thisWidget = do
  menuOpenE <- divClass "header-wrapper" $ headerAndroid titleD
  menuAndroid menuOpenE thisWidget

headerDesktop :: MonadFront t m => Dynamic t Text -> m (Dynamic t Bool)
headerDesktop titleD = divClass "header header-black" $ mdo
  stD <- getRetractStack
  backButton "header-button" $ null <$> stD
  divClass "header-tir-text" $ dynText titleD
  menuBtnE <- divButton "header-button" $ elClassDyn "i" menuButtonIconClassD blank
  let menuButtonIconClassD = menuButtonIconClass <$> menuIsHiddenD
  menuIsHiddenD <- toggle True menuBtnE
  pure menuIsHiddenD

headerAndroid :: MonadFront t m => Dynamic t Text -> m (Event t ())
headerAndroid titleD = divClass "header header-black" $ mdo
  stD <- getRetractStack
  backButton "header-button" $ null <$> stD
  divClass "header-tir-text" $ dynText titleD
  divButton "header-button" $ elClass "i" "fas fa-bars fa-fw" blank

menuDesktop :: MonadFront t m => Dynamic t Bool -> Maybe (Dynamic t (m ())) -> m ()
menuDesktop menuIsHiddenD thisWidget = do
  let menuClassesD = visibilityClass "menu" <$> menuIsHiddenD
  divClassDyn menuClassesD $ do
    ps <- getPubStorage
    pure ()

menuAndroid :: MonadFront t m => Event t () -> Maybe (Dynamic t (m ())) -> m ()
menuAndroid menuOpenE thisWidget = mdo
  menuIsHiddenD <- holdDyn True $ leftmost [False <$ menuOpenE, True <$ closeMenuE]
  let menuWrapperClassesD = visibilityClass "menu-android-wrapper" <$> menuIsHiddenD
      menuClassesD = visibilityClass "menu-android" <$> menuIsHiddenD
  closeMenuE <- divClassDyn menuWrapperClassesD $ divClassDyn menuClassesD $ do
    ps <- getPubStorage
    menuBtnE <- divClass "menu-android-header" $ divButton "menu-android-close-button header-button" $ elClass "i" "fas fa-times fa-fw" blank
    pure menuBtnE
  pure ()

-- | Button for going back on widget history
backButton :: MonadFrontBase t m => Text -> Dynamic t Bool -> m ()
backButton classes isHiddenD = do
  let backButtonClassesD = visibilityClass classes <$> isHiddenD
  e <- divButton backButtonClassesD $ elClass "i" "fas fa-arrow-left fa-fw" blank
  void $ retract e

menuButtonIconClass :: Bool -> Text
menuButtonIconClass True = "fas fa-bars fa-fw"
menuButtonIconClass False = "fas fa-times fa-fw"

-- | Appends "hidden" class to the given classes if the second argument equals True
visibilityClass :: Text -> Bool -> Text
visibilityClass classes True = classes <> " hidden"
visibilityClass classes False = classes
