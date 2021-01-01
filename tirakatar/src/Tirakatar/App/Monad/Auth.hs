{-# LANGUAGE UndecidableInstances #-}
module Tirakatar.App.Monad.Auth(
    liftAuth
  , liftUnauthed
  ) where

import Control.Concurrent
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.STM
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Network.Socket
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import System.Directory

import Tirakatar.App.Language
import Tirakatar.App.Log.Types
import Tirakatar.App.Monad.Async
import Tirakatar.App.Monad.Client
import Tirakatar.App.Monad.Front
import Tirakatar.App.Monad.Storage
import Tirakatar.App.Native
import Tirakatar.App.Platform
import Tirakatar.App.Settings (Settings(..))
import Tirakatar.App.Storage.Util
import Tirakatar.App.Status.Types
import Tirakatar.App.Version
import Tirakatar.App.Types
import Tirakatar.Types

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

data Env t = Env {
  -- Unauth context's fields
  env'settings        :: !(ExternalRef t Settings)
, env'pauseEF         :: !(Event t (), IO ())
, env'resumeEF        :: !(Event t (), IO ())
, env'backEF          :: !(Event t (), IO ())
, env'loading         :: !(Event t (Bool, Text), (Bool, Text) -> IO ())
, env'langRef         :: !(ExternalRef t Language)
, env'storeDir        :: !Text
, env'alertsEF        :: (Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alert event and trigger
, env'logsTrigger     :: (Event t LogEntry, LogEntry -> IO ())
, env'logsNameSpaces  :: !(ExternalRef t [Text])
, env'uiChan          :: !(Chan (IO ()))
, env'passModalEF     :: !(Event t (Int, Text), (Int, Text) -> IO ())
, env'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
-- Auth context
, env'authRef         :: !(ExternalRef t AuthInfo)
, env'logoutFire      :: !(IO ())
, env'statusUpdates   :: !(ExternalRef t StatusUpdate)
, env'storeMutex      :: !(MVar ())
, env'storeChan       :: !(TChan (Text, AuthInfo))
}

type ErgveinM t m = ReaderT (Env t) m

instance Monad m => HasStoreDir (ErgveinM t m) where
  getStoreDir = asks env'storeDir
  {-# INLINE getStoreDir #-}

instance MonadBaseConstr t m => MonadEgvLogger t (ErgveinM t m) where
  getLogsTrigger = asks env'logsTrigger
  {-# INLINE getLogsTrigger #-}
  getLogsNameSpacesRef = asks env'logsNameSpaces
  {-# INLINE getLogsNameSpacesRef #-}

instance MonadBaseConstr t m => MonadLocalized t (ErgveinM t m) where
  setLanguage lang = do
    langRef <- asks env'langRef
    writeExternalRef langRef lang
  {-# INLINE setLanguage #-}
  setLanguageE langE = do
    langRef <- asks env'langRef
    performEvent_ $ fmap (writeExternalRef langRef) langE
  {-# INLINE setLanguageE #-}
  getLanguage = externalRefDynamic =<< asks env'langRef
  {-# INLINE getLanguage #-}

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives, HasVersion) => MonadFrontBase t (ErgveinM t m) where
  getLoadingWidgetTF = asks env'loading
  {-# INLINE getLoadingWidgetTF #-}
  getPauseEventFire = asks env'pauseEF
  {-# INLINE getPauseEventFire #-}
  getResumeEventFire = asks env'resumeEF
  {-# INLINE getResumeEventFire #-}
  getBackEventFire = asks env'backEF
  {-# INLINE getBackEventFire #-}
  getUiChan = asks env'uiChan
  {-# INLINE getUiChan #-}
  getLangRef = asks env'langRef
  {-# INLINE getLangRef #-}
  getAuthInfoMaybeRef = fmapExternalRef Just =<< asks env'authRef
  {-# INLINE getAuthInfoMaybeRef #-}
  getPasswordModalEF = asks env'passModalEF
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = asks env'passSetEF
  {-# INLINE getPasswordSetEF #-}
  setAuthInfo e = do
    authRef <- asks env'authRef
    fire <- asks env'logoutFire
    performEvent $ ffor e $ \case
      Nothing -> do
        logWrite "authed setAuthInfo: logout"
        setLastStorage Nothing
        liftIO fire
      Just v -> do
        logWrite "authed setAuthInfo: changing auth info"
        setLastStorage $ Just . _storage'name . _authInfo'storage $ v
        writeExternalRef authRef v
  {-# INLINE setAuthInfo #-}

instance MonadBaseConstr t m => MonadHasSettings t (ErgveinM t m) where
  getSettingsRef = asks env'settings
  {-# INLINE getSettingsRef #-}

instance MonadFrontBase t m => MonadFrontAuth t (ErgveinM t m) where
  getAuthInfoRef = asks env'authRef
  {-# INLINE getAuthInfoRef #-}
  getStatusUpdRef = asks env'statusUpdates
  {-# INLINE getStatusUpdRef #-}

instance MonadBaseConstr t m => MonadIndexClient t (ErgveinM t m) where
  getActiveAddrsRef = pure ()
  {-# INLINE getActiveAddrsRef #-}

instance MonadBaseConstr t m => MonadAlertPoster t (ErgveinM t m) where
  postAlert e = do
    (_, fire) <- asks env'alertsEF
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . env'alertsEF)
  getAlertEventFire = asks env'alertsEF
  {-# INLINE postAlert #-}
  {-# INLINE newAlertEvent #-}
  {-# INLINE getAlertEventFire #-}

instance (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t (ErgveinM t m) where
  getEncryptedPrvStorage = fmap (_storage'encryptedPrvStorage . _authInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getEncryptedPrvStorage #-}
  getStorageName = fmap (_storage'name . _authInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getStorageName #-}
  getPubStorage = fmap (_storage'pubStorage . _authInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getPubStorage #-}
  getPubStorageD = do
    authInfoD <- externalRefDynamic =<< asks env'authRef
    pure $ ffor authInfoD $ \ai -> ai ^. authInfo'storage . storage'pubStorage
  {-# INLINE getPubStorageD #-}
  storeStorage caller e = do
    ref <-  asks env'authRef
    performEvent $ ffor e $ \_ -> do
        authInfo <- readExternalRef ref
        let storage = _authInfo'storage authInfo
        let eciesPubKey = _authInfo'eciesPubKey authInfo
        saveStorageToFile caller eciesPubKey storage
  {-# INLINE storeStorage #-}

  modifyPubStorage caller fe = do
    authRef   <- asks env'authRef
    chan      <- asks env'storeChan
    performEvent $ ffor fe $ \f -> do
      mai <- modifyExternalRefMaybe authRef $ \ai ->
        let mps' = f (ai ^. authInfo'storage . storage'pubStorage)
        in (\a -> (a, a)) . (\ps' -> ai & authInfo'storage . storage'pubStorage .~ ps') <$> mps'
      liftIO $ atomically $ traverse_ (writeTChan chan . (caller, )) mai
  {-# INLINE modifyPubStorage #-}
  getStoreMutex = asks env'storeMutex
  {-# INLINE getStoreMutex #-}
  getStoreChan = asks env'storeChan
  {-# INLINE getStoreChan #-}

-- | Minimum time between two writes of storage to disk
storeTimeBetweenWrites :: NominalDiffTime
storeTimeBetweenWrites = 20

-- | Thread that writes down updates of app storages and checks that write down doesn't occur too frequent.
storageStoreThread :: PlatformNatives => Text -> MVar () -> TChan (Text, AuthInfo) -> IO ()
storageStoreThread storeDir mutex updChan = void $ forkOnOther $ do
  timeRef <- newTVarIO =<< getCurrentTime
  lastUpdTimeRef <- newTVarIO =<< getCurrentTime
  lastStoreRef <- newTVarIO Nothing
  -- Thread that updates reference with time to compare it with value in lastUpdTimeRef in getTimedWrite
  void $ forkIO $ fix $ \next -> do
    threadDelay $ ceiling storeTimeBetweenWrites
    currTime <- getCurrentTime
    atomically $ writeTVar timeRef currTime
    next
  -- Thread that reads from chan and stores last storage to reference which next thread will check and validate
  -- against timeout.
  void $ forkIO $ fix $ \next -> do
    atomically $ do
      val <- readTChan updChan
      writeTVar lastStoreRef $ Just val
    next
  -- If we have awaiting write to disk and time passed > timeout we return the value unless retry
  let getTimedWrite = do
        mval <- readTVar lastStoreRef
        case mval of
          Nothing -> retry
          Just val -> do
            currTime <- readTVar timeRef
            updTime <- readTVar lastUpdTimeRef
            when (diffUTCTime currTime updTime < storeTimeBetweenWrites) retry
            writeTVar lastUpdTimeRef currTime
            writeTVar lastStoreRef Nothing
            pure val
  -- Thread that indefinetely queries if we need to write down new state
  fix $ \next -> do
    (caller, authInfo) <- atomically getTimedWrite
    storeWalletIO caller storeDir mutex authInfo
    next

storeWalletIO :: PlatformNatives => Text -> Text -> MVar () -> AuthInfo -> IO ()
storeWalletIO caller storeDir mutex ai = do
  let storage = _authInfo'storage ai
  let eciesPubKey = _authInfo'eciesPubKey ai
  withMVar mutex $ const $ flip runReaderT storeDir $ saveStorageToFile caller eciesPubKey storage

-- | Execute action under authorized context or return the given value as result
-- if user is not authorized. Each time the login info changes and authInfo'isUpdate flag is set to 'False'
-- (user logs out or logs in) the widget is updated.
liftAuth :: MonadFrontBase t m => m a -> ErgveinM t m a -> m (Dynamic t a)
liftAuth ma0 ma = mdo
  mauthD <- getAuthInfoMaybe
  mauth0 <- sample . current $ mauthD
  (logoutE, logoutFire) <- newTriggerEvent
  let runAuthed auth = do
        -- Get refs from Unauth context
        pauseEF         <- getPauseEventFire
        resumeEF        <- getResumeEventFire
        backEF          <- getBackEventFire
        loading         <- getLoadingWidgetTF
        langRef         <- getLangRef
        storeDir        <- getStoreDir
        alertsEF        <- getAlertEventFire
        logsTrigger     <- getLogsTrigger
        logsNameSpaces  <- getLogsNameSpacesRef
        uiChan          <- getUiChan
        passModalEF     <- getPasswordModalEF
        passSetEF       <- getPasswordSetEF
        settingsRef     <- getSettingsRef

        authRef         <- newExternalRef auth

        storeMutex      <- liftIO $ newMVar ()
        storeChan       <- liftIO newTChanIO
        statRef         <- newExternalRef NotActive
        let env = Env {
                env'settings = settingsRef
              , env'pauseEF = pauseEF
              , env'resumeEF = resumeEF
              , env'backEF = backEF
              , env'loading = loading
              , env'langRef = langRef
              , env'storeDir = storeDir
              , env'alertsEF = alertsEF
              , env'logsTrigger = logsTrigger
              , env'logsNameSpaces = logsNameSpaces
              , env'uiChan = uiChan
              , env'passModalEF = passModalEF
              , env'passSetEF = passSetEF
              , env'authRef = authRef
              , env'logoutFire = logoutFire ()
              , env'storeMutex = storeMutex
              , env'storeChan = storeChan
              , env'statusUpdates = statRef
              }

        flip runReaderT env $ do -- Workers and other routines go here
          liftIO $ storageStoreThread storeDir storeMutex storeChan
          when isAndroid (deleteTmpFiles storeDir)
          pure ()
        runReaderT (wrapped "liftAuth" ma) env
  let
    ma0' = maybe ma0 runAuthed mauth0
    newAuthInfoE = ffilter isMauthUpdate $ updated mauthD
    redrawE = leftmost [newAuthInfoE, Nothing <$ logoutE]
  widgetHold ma0' $ ffor redrawE $ maybe ma0 runAuthed

isMauthUpdate :: Maybe AuthInfo -> Bool
isMauthUpdate mauth = case mauth of
  Nothing -> True
  Just auth -> not $ _authInfo'isUpdate auth

-- | Lift action that doesn't require authorisation in context where auth is mandatory
liftUnauthed :: m a -> ErgveinM t m a
liftUnauthed ma = ReaderT $ const ma

wrapped :: MonadFrontBase t m => Text -> ErgveinM t m a -> ErgveinM t m a
wrapped caller ma = do
  void $ storeStorage clr =<< getPostBuild
  ma
  where clr = caller <> ":" <> "wrapped"

-- Deletes files created with 'atomicWriteFile' from specified directiry
deleteTmpFiles :: MonadIO m => Text -> m ()
deleteTmpFiles dir = liftIO $ do
  entries <- listDirectory $ T.unpack dir
  traverse_ removeFile $ L.filter isTmpFile entries
  where isTmpFile filePath = "atomic" `L.isPrefixOf` filePath && ".write" `L.isSuffixOf` filePath
