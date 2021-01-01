module Tirakatar.Server.Environment
  (
    ServerEnv(..)
  , newServerEnv
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Monad.Reader
import System.IO

import Tirakatar.Server.Config

data ServerEnv = ServerEnv {
  envServerConfig :: !Config
, envLogger       :: !(Chan (Loc, LogSource, LogLevel, LogStr))
, envShutdownFlag :: !(TVar Bool)
}

newServerEnv :: (MonadIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m)
  => Config -> m ServerEnv
newServerEnv cfg = do
  logger <- liftIO newChan
  liftIO $ hSetBuffering stdout LineBuffering
  void $ liftIO $ forkIO $ runStdoutLoggingT $ unChanLoggingT logger
  shutdownVar <- liftIO $ newTVarIO False
  pure ServerEnv
    { envServerConfig = cfg
    , envLogger       = logger
    , envShutdownFlag = shutdownVar
    }
