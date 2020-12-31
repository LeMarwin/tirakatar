module Tirakatar.App.Monad.Unauth
  (
    UnauthEnv(..)
  , newEnv
  , runEnv
  ) where

import Control.Concurrent.Chan
import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.Socket (SockAddr)
import Reflex.Dom.Retractable
import Reflex.ExternalRef

import Tirakatar.App.Language
import Tirakatar.App.Log.Types
import Tirakatar.App.Monad.Front
import Tirakatar.App.Monad.Util
import Tirakatar.App.Native
import Tirakatar.App.Run.Callbacks
import Tirakatar.App.Settings
import Tirakatar.App.Storage.Util
import Tirakatar.App.Version

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data UnauthEnv t = UnauthEnv {
  unauth'settings        :: !(ExternalRef t Settings)
, unauth'pauseEF         :: !(Event t (), IO ())
, unauth'resumeEF        :: !(Event t (), IO ())
, unauth'backEF          :: !(Event t (), IO ())
, unauth'loading         :: !(Event t (Bool, Text), (Bool, Text) -> IO ())
, unauth'langRef         :: !(ExternalRef t Language)
, unauth'storeDir        :: !Text
, unauth'alertsEF        :: !(Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alerts event and trigger
, unauth'logsTrigger     :: !(Event t LogEntry, LogEntry -> IO ())
, unauth'logsNameSpaces  :: !(ExternalRef t [Text])
, unauth'uiChan          :: !(Chan (IO ()))
, unauth'authRef         :: !(ExternalRef t (Maybe AuthInfo))
, unauth'passModalEF     :: !(Event t (Int, Text), (Int, Text) -> IO ())
, unauth'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
-- Client context
}

type UnauthM t m = ReaderT (UnauthEnv t) m

instance Monad m => HasStoreDir (UnauthM t m) where
  getStoreDir = asks unauth'storeDir
  {-# INLINE getStoreDir #-}

instance MonadBaseConstr t m => MonadEgvLogger t (UnauthM t m) where
  getLogsTrigger = asks unauth'logsTrigger
  {-# INLINE getLogsTrigger #-}
  getLogsNameSpacesRef = asks unauth'logsNameSpaces
  {-# INLINE getLogsNameSpacesRef #-}

instance MonadBaseConstr t m => MonadLocalized t (UnauthM t m) where
  setLanguage lang = do
    langRef <- asks unauth'langRef
    writeExternalRef langRef lang
  {-# INLINE setLanguage #-}
  setLanguageE langE = do
    langRef <- asks unauth'langRef
    performEvent_ $ fmap (writeExternalRef langRef) langE
  {-# INLINE setLanguageE #-}
  getLanguage = externalRefDynamic =<< asks unauth'langRef
  {-# INLINE getLanguage #-}

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives, HasVersion) => MonadFrontBase t (UnauthM t m) where
  getLoadingWidgetTF = asks unauth'loading
  {-# INLINE getLoadingWidgetTF #-}
  getPauseEventFire = asks unauth'pauseEF
  {-# INLINE getPauseEventFire #-}
  getResumeEventFire = asks unauth'resumeEF
  {-# INLINE getResumeEventFire #-}
  getBackEventFire = asks unauth'backEF
  {-# INLINE getBackEventFire #-}
  getUiChan = asks unauth'uiChan
  {-# INLINE getUiChan #-}
  getLangRef = asks unauth'langRef
  {-# INLINE getLangRef #-}
  getAuthInfoMaybeRef = asks unauth'authRef
  {-# INLINE getAuthInfoMaybeRef #-}
  setAuthInfo e = do
    authRef <- asks unauth'authRef
    performEvent $ ffor e $ \v -> do
      logWrite "unauthed setAuthInfo: setting info"
      -- setLastStorage $ _storage'walletName . _authInfo'storage <$> v
      writeExternalRef authRef v
  {-# INLINE setAuthInfo #-}
  getPasswordModalEF = asks unauth'passModalEF
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = asks unauth'passSetEF
  {-# INLINE getPasswordSetEF #-}

instance MonadBaseConstr t m => MonadHasSettings t (UnauthM t m) where
  getSettingsRef = asks unauth'settings
  {-# INLINE getSettingsRef #-}

instance MonadBaseConstr t m => MonadAlertPoster t (UnauthM t m) where
  postAlert e = do
    (_, fire) <- asks unauth'alertsEF
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . unauth'alertsEF)
  getAlertEventFire = asks unauth'alertsEF
  {-# INLINE postAlert #-}
  {-# INLINE newAlertEvent #-}
  {-# INLINE getAlertEventFire #-}

instance MonadBaseConstr t m => MonadIndexClient t (UnauthM t m) where
  getActiveAddrsRef = pure ()
  {-# INLINE getActiveAddrsRef #-}

newEnv :: MonadBaseConstr t m
  => Settings
  -> Chan (IO ()) -- UI callbacks channel
  -> m (UnauthEnv t)
newEnv settings uiChan = do
  settingsRef <- newExternalRef settings
  (pauseE, pauseFire) <- newTriggerEvent
  (resumeE, resumeFire) <- newTriggerEvent
  (backE, backFire) <- newTriggerEvent
  loadingEF <- newTriggerEvent
  alertsEF <- newTriggerEvent
  passSetEF <- newTriggerEvent
  passModalEF <- newTriggerEvent
  authRef <- newExternalRef Nothing
  langRef <- newExternalRef $ settingsLang settings
  logsTrigger <- newTriggerEvent
  nameSpaces <- newExternalRef []

  let env = UnauthEnv {
          unauth'settings         = settingsRef
        , unauth'pauseEF          = (pauseE, pauseFire ())
        , unauth'resumeEF         = (resumeE, resumeFire ())
        , unauth'backEF           = (backE, backFire ())
        , unauth'loading          = loadingEF
        , unauth'langRef          = langRef
        , unauth'storeDir         = settingsStoreDir settings
        , unauth'alertsEF         = alertsEF
        , unauth'logsTrigger      = logsTrigger
        , unauth'logsNameSpaces   = nameSpaces
        , unauth'uiChan           = uiChan
        , unauth'authRef          = authRef
        , unauth'passModalEF      = passModalEF
        , unauth'passSetEF        = passSetEF
        }
  flip runReaderT env $ do
    pure ()
  pure env

runEnv :: (MonadBaseConstr t m, PlatformNatives, HasVersion)
  => RunCallbacks -> UnauthEnv t -> ReaderT (UnauthEnv t) (RetractT t m) a -> m a
runEnv cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ (snd . unauth'backEF) e
  liftIO $ writeIORef (runPauseCallback cbs) $ (snd . unauth'pauseEF) e
  liftIO $ writeIORef (runResumeCallback cbs) $ (snd . unauth'resumeEF) e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = void (retract . fst =<< getBackEventFire) >> ma
