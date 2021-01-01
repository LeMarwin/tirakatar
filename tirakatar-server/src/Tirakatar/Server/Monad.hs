{-# LANGUAGE DerivingVia #-}
module Tirakatar.Server.Monad where

import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Tirakatar.Server.Environment
import Tirakatar.Server.Monad.Prim

newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader ServerEnv, MonadThrow, MonadCatch, MonadMask, MonadBase IO)

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT ServerEnv (LoggingT IO)) a }

instance MonadBaseControl IO ServerM where
  type StM ServerM a = StMServerM a
  liftBaseWith f = ServerM $ liftBaseWith $ \q -> f (fmap StMServerM . q . unServerM)
  restoreM = ServerM . restoreM . unStMServerM

runServerMIO :: ServerEnv -> ServerM a -> IO a
runServerMIO e = runChanLoggingT (envLogger e) . flip runReaderT e . unServerM

instance HasShutdownFlag ServerM where
  getShutdownFlag = asks envShutdownFlag
  {-# INLINE getShutdownFlag #-}

instance MonadUnliftIO ServerM where
  askUnliftIO = ServerM $ (\(UnliftIO run) -> UnliftIO $ run . unServerM) <$> askUnliftIO
  withRunInIO go = ServerM $ withRunInIO (\k -> go $ k . unServerM)

stopThreadIfShutdown :: Thread -> ServerM ()
stopThreadIfShutdown thread = do
  shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
  when shutdownFlag $ liftIO $ stop thread
