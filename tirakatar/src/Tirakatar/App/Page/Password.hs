{-# LANGUAGE CPP #-}
module Tirakatar.App.Page.Password(
    setupPasswordPage
  , setupLoginPage
  , setupPatternPage
  , askPasswordPage
  , askTextPasswordPage
  , changePasswordWidget
  ) where

import Reflex.ExternalRef
import Reflex.Localize
import Text.Read

import Tirakatar.Crypto.Keys     (Mnemonic)
import Tirakatar.Text
import Tirakatar.Types
import Tirakatar.App.Alert
import Tirakatar.App.Elements
import Tirakatar.App.Elements.Input
import Tirakatar.App.Language
import Tirakatar.App.Localization.Password
-- import Tirakatar.App.Localization.Restore
import Tirakatar.App.Monad
import Tirakatar.App.Native
import Tirakatar.App.Password
import Tirakatar.App.Platform
import Tirakatar.App.Storage.AuthInfo
import Tirakatar.App.Wrapper

import qualified Data.Text as T

setupPasswordPage :: MonadFrontBase t m => AccountSource -> Mnemonic -> Maybe Text -> m ()
setupPasswordPage accSource mnemonic mlogin = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    logPassE <- setupLoginPassword mlogin btnE
    btnE <- submitSetBtn
  void $ nextWidget $ ffor logPassE $ \(login, pass) -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage accSource mnemonic login True
        else performAuth accSource mnemonic login pass True
    , retractablePrev = if pass == ""
        then Just $ pure $ setupPasswordPage accSource mnemonic (Just login)
        else Nothing
    }

confirmEmptyPage :: MonadFrontBase t m
  => AccountSource
  -> Mnemonic
  -> Text
  -> Bool
  -> m ()
confirmEmptyPage accSource mnemonic login isPass = wrapperSimple True $ do
  h4 $ localizedText CEPAttention
  h5 $ localizedText CEPConsequences
  divClass "fit-content ml-a mr-a" $ do
    setE <- divClass "" (submitClass "button button-outline w-100" PWSSet)
    void $ retract =<< divClass "" (submitClass "button button-outline w-100" CEPBack)
    void $ nextWidget $ ffor setE $ const $ Retractable {
        retractableNext = performAuth accSource mnemonic login "" isPass
      , retractablePrev = Nothing
      }

performAuth :: MonadFrontBase t m
  => AccountSource
  -> Mnemonic
  -> Text
  -> Password
  -> Bool
  -> m ()
performAuth accSource mnemonic login pass isPass = do
  goE <- getPostBuild
  storageE <- performEvent $ ffor goE $ const $
    initAuthInfo accSource mnemonic login pass isPass
  authInfoE <- handleDangerMsg storageE
  void $ setAuthInfo $ Just <$> authInfoE

setupLoginPage :: MonadFrontBase t m => AccountSource -> Mnemonic -> m ()
setupLoginPage accSource mnemonic = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText LPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText LPSDescr
  rec
    loginE <- setupLogin btnE
    btnE <- submitSetBtn
  void $ nextWidget $ ffor loginE $ \l -> Retractable {
      retractableNext = setupPatternPage accSource mnemonic l
    , retractablePrev = Just $ pure $ setupLoginPage accSource mnemonic
    }

setupPatternPage :: MonadFrontBase t m
  => AccountSource
  -> Mnemonic
  -> Text
  -> m ()
setupPatternPage accSource mnemonic l = wrapperSimple True $ do
  let this = Just $ pure $ setupPatternPage accSource mnemonic l
  divClass "password-setup-title" $ h4 $ localizedText PatPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PatPSDescr
  patE <- setupPattern
  (setPassE, skipE) <- divClass "fit-content ml-a mr-a" $ do
    setPassE <- divClass "" $ submitClass "button button-outline w-100" PatPSPass
    skipE <- divClass "" $ submitClass "button button-outline w-100" CEPSkip
    pure (setPassE, skipE)
  let passE = leftmost ["" <$ skipE, patE]
  void $ nextWidget $ ffor passE $ \pass -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage accSource mnemonic l False
        else performAuth accSource mnemonic l pass False
    , retractablePrev = if pass == "" then this else Nothing
    }
  void $ nextWidget $ ffor setPassE $ const $ Retractable {
      retractableNext = setupMobilePasswordPage accSource mnemonic l
    , retractablePrev = this
    }

setupMobilePasswordPage :: MonadFrontBase t m
  => AccountSource
  -> Mnemonic
  -> Text
  -> m ()
setupMobilePasswordPage accSource mnemonic l = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSPassTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    passE <- setupPassword btnE
    btnE <- divClass "fit-content ml-a mr-a" $ do
      btnE' <- divClass "" $ (submitClass "button button-outline w-100" PWSSet)
      setPattE <- divClass "" $ submitClass "button button-outline w-100" PatPSPatt
      void $ retract setPattE
      pure btnE'
  void $ nextWidget $ ffor passE $ \pass -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage accSource mnemonic pass True
        else performAuth accSource mnemonic l pass True
    , retractablePrev = if pass == ""
        then Just $ pure $ setupMobilePasswordPage accSource mnemonic l
        else Nothing
    }

askPasswordPage :: MonadFrontBase t m => Text -> m (Event t Password)
askPasswordPage name = wrapperSimple True $ askPassword name True

askTextPasswordPage :: (MonadFrontBase t m, LocalizedPrint l1, LocalizedPrint l2) => l1 -> l2 -> m (Event t Password)
askTextPasswordPage title description = wrapperSimple True $ askTextPassword title description

data ChangePasswordStrings = CPSTitle | CPSDescr | CPSOld

instance LocalizedPrint (Bool, ChangePasswordStrings) where
  localizedShow l (b, v) = case l of
    English -> let s = if b then "password" else "key" in case v of
      CPSTitle -> "Change " <> s
      CPSDescr -> "Enter the new " <> s <> " first"
      CPSOld   -> "You will have to enter the old " <> s <> " at the end"
    Russian -> let s = if b then "пароль" else "ключ" in case v of
      CPSTitle -> "Смена " <> if b then "пароля" else "ключа"
      CPSDescr -> "Введите новый " <> s
      CPSOld   -> "В конце вам понадобится ввести старый " <> s

changePasswordWidget :: MonadFront t m => m (Event t (Password, Bool))
changePasswordWidget =
  if isAndroid then changePasswordMobileWidget else changePasswordDescWidget
{-# INLINE changePasswordWidget #-}


changePasswordDescWidget :: MonadFront t m => m (Event t (Password, Bool))
changePasswordDescWidget = wrapperSimple True $ mdo
  tglD <- holdDyn False tglE
  (passE, tglE) <- fmap switchDyn2 $ widgetHoldDyn $ ffor tglD $ \case
    True  -> (fmap . fmap) (False <$) confirmEmptyWidget
    False -> mdo
      changePasswordDescr True
      passE' <- setupPassword btnE
      btnE <- submitSetBtn
      let (emptyE, passE'') = splitFilter (== "") passE'
      pure (passE'', True <$ emptyE)
  pure $ (,True) <$> passE

changePasswordDescr :: (MonadLocalized t m, DomBuilder t m, PostBuild t m) => Bool -> m ()
changePasswordDescr b = do
  divClass "password-setup-title" $ h4 $ localizedText (b, CPSTitle)
  divClass "password-setup-descr" $ h5 $ localizedText (b, CPSDescr)
  divClass "password-setup-descr" $ h5 $ localizedText (b, CPSOld)

confirmEmptyWidget :: MonadFront t m => m (Event t Password, Event t ())
confirmEmptyWidget = do
  h4 $ localizedText CEPAttention
  h5 $ localizedText CEPConsequences
  divClass "fit-content ml-a mr-a" $ do
    setE <- divClass "" (submitClass "button button-outline w-100" PWSSet)
    backE <- divClass "" (submitClass "button button-outline w-100" CEPBack)
    pure ("" <$ setE, backE)

data CPMStage = CPMPattern | CPMPassword | CPMEmpty Bool

changePasswordMobileWidget :: MonadFront t m => m (Event t (Password, Bool))
changePasswordMobileWidget = wrapperSimple True $ mdo
  login <- fmap (_authInfo'login) $ readExternalRef =<< getAuthInfoRef
  let name = T.replace " " "_" login
  stage0 <- fmap eitherToStage $ retrieveValue ("meta_wallet_" <> name) False
  stageD <- holdDyn stage0 nextE
  (patE, nextE) <- fmap switchDyn2 $ widgetHoldDyn $ ffor stageD $ \case
    CPMEmpty b -> do
      (passE, backE) <- confirmEmptyWidget
      pure ((,b) <$> passE, boolToStage b <$ backE)
    CPMPassword -> mdo
      changePasswordDescr True
      passE' <- setupPassword btnE
      (btnE, setPattE) <- divClass "fit-content ml-a mr-a" $ do
        btnE' <- divClass "" $ (submitClass "button button-outline w-100" PWSSet)
        setPattE' <- divClass "" $ submitClass "button button-outline w-100" PatPSPatt
        pure (btnE', setPattE')
      let (emptyE, passE) = splitFilter (== "") passE'
      let nxtE = leftmost [CPMEmpty True <$ emptyE, CPMPattern <$ setPattE]
      pure ((, True) <$> passE, nxtE)
    CPMPattern -> do
      changePasswordDescr False
      patE' <- setupPattern
      divClass "fit-content ml-a mr-a" $ do
        setPassE <- divClass "" $ submitClass "button button-outline w-100" PatPSPass
        skipE <- divClass "" $ submitClass "button button-outline w-100" CEPSkip
        let nxtE = leftmost [CPMPassword <$ setPassE, CPMEmpty False <$ skipE]
        pure ((, False) <$> patE', nxtE)
  pure patE
  where
    boolToStage b = if b then CPMPassword else CPMPattern
    eitherToStage = either (const CPMPattern) boolToStage
