module Tirakatar.Server.App (
    app
  ) where

import Control.Concurrent.STM.TVar
import Control.Immortal
import Control.Monad.STM
import System.Posix.Signals
import Control.Monad.IO.Unlift
import Control.Monad.Logger

import Tirakatar.Server.Config
import Tirakatar.Server.Environment
import Tirakatar.Server.Monad
import Tirakatar.Server.Utils
import Tirakatar.Text

import qualified Data.Text.IO as T

onStartup :: ServerM [Thread]
onStartup = do
  pure []

onShutdown :: ServerEnv -> IO ()
onShutdown env = do
  T.putStrLn "Server stop signal recivied..."
  T.putStrLn "service is stopping"
  atomically $ writeTVar (envShutdownFlag env) True

finalize :: (MonadIO m, MonadLogger m) => ServerEnv -> [Thread] -> m ()
finalize _ _ = do
  logInfoN "Waiting for threads to close"
  -- liftIO $ sequence_ $ wait <$> workerTreads
  logInfoN "service is stopped"

app :: (MonadUnliftIO m, MonadLogger m) => ServerEnv -> m ()
app env = do
  let portText = showt $ cfgServerPort $ envServerConfig env
  workerThreads <- liftIO $ runServerMIO env onStartup
  logInfoN $ "Server started at:" <> portText
  _ <- liftIO $ installHandler sigTERM (Catch $ onShutdown env) Nothing
  liftIO $ cancelableDelay (envShutdownFlag env) (-1)
  finalize env workerThreads
