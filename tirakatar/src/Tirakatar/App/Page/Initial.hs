{-# LANGUAGE CPP #-}
module Tirakatar.App.Page.Initial(
    initialPage
  ) where

import Tirakatar.Types.Storage
import Tirakatar.App.Alert
import Tirakatar.App.Elements
import Tirakatar.App.Language
import Tirakatar.App.Localization.Initial
import Tirakatar.App.Monad
import Tirakatar.App.Native
import Tirakatar.App.Page.Password
import Tirakatar.App.Page.Settings.Unauth
import Tirakatar.App.Page.Seed
import Tirakatar.App.Platform
import Tirakatar.App.Storage.AuthInfo
import Tirakatar.App.Storage.Util
import Tirakatar.App.Wrapper

import Tirakatar.App.Page.PatternKey
import qualified Data.Map.Strict as M

import Tirakatar.Text

data GoPage = GoSeed | GoRestore | GoSettings

initialPage :: MonadFrontBase t m => Bool -> m ()
initialPage redir = do
  logWrite "Initial page rendering"
  ss <- listStorages
  logWrite $ showt ss
  if null ss then noAccountsPage else hasAccountsPage redir ss
  logWrite "Finished initial page rendering"

noAccountsPage :: MonadFrontBase t m => m ()
noAccountsPage = wrapperSimple True $ divClass "initial-page-options" $ createRestore

createRestore :: MonadFrontBase t m => m ()
createRestore = do
  let items = [(GoSeed, IPSCreate), (GoRestore, IPSRestore), (GoSettings, IPSSettings)]
  goE <- fmap leftmost $ flip traverse items $ \(act, lbl) ->
    fmap (act <$) $ outlineButton lbl
  void $ nextWidget $ ffor goE $ \go -> Retractable {
      retractableNext = case go of
        GoSeed -> mnemonicPage
        GoRestore -> seedRestorePage
        GoSettings -> settingsPageUnauth
    , retractablePrev = Just $ pure $ initialPage False
    }

hasAccountsPage :: MonadFrontBase t m => Bool -> [StorageName] -> m ()
hasAccountsPage redir ss = do
  buildE <- getPostBuild
  mnameE <- performEvent $ getLastStorage <$ buildE
  void $ nextWidget $ ffor mnameE $ \mname -> Retractable {
      retractableNext = maybe (selectWalletsPage ss) selectNext mname
    , retractablePrev = Just $ pure $ initialPage False
    }
  where
    selectNext = if redir then loadWalletPage else const (selectWalletsPage ss)

selectWalletsPage :: MonadFrontBase t m => [StorageName] -> m ()
selectWalletsPage ss = wrapperSimple True $ divClass "initial-page-options" $ do
  h4 $ localizedText IPSSelectWallet
  flip traverse_ ss $ \name -> do
    btnE <- outlineButton name
    void $ nextWidget $ ffor btnE $ const $ Retractable {
        retractableNext = loadWalletPage name
      , retractablePrev = Just $ pure $ selectWalletsPage ss
      }
  h4 $ localizedText IPSOtherOptions
  createRestore

loadWalletPage :: forall t m . MonadFrontBase t m => StorageName -> m ()
loadWalletPage name = do
  buildE <- getPostBuild
  mPlainE <- performEvent $ loadAuthInfo name "" <$ buildE
  let authE' = fmapMaybe (either (const Nothing) Just) mPlainE
  authE'' <- fmap switchDyn $ widgetHold (pure never) $ ffor mPlainE $ \case
    Right _ -> pure never
    Left _ -> do
      passE <- askPasswordPage name
      mOldAuthE <- performEvent $ loadAuthInfo name <$> passE
      handleDangerMsg mOldAuthE
  let authE  = leftmost [authE', authE'']
  when isAndroid $ performEvent_ $ ffor authE $ const $ do
    c <- loadCounter
    saveCounter $ PatternTries $ M.insert name 0 (patterntriesCount c)
  void $ setAuthInfo $ Just <$> authE
