module Tirakatar.Server.Config
  (
    Config(..)
  , HasServerConfig(..)
  , loadConfig
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Yaml.Config

data Config = Config {
  cfgServerPort               :: !Int
, cfgServerTcpPort            :: !Int
, cfgServerHostname           :: !String
}

class HasServerConfig m where
  serverConfig :: m Config

instance Monad m => HasServerConfig (ReaderT Config m) where
  serverConfig = ask
  {-# INLINE serverConfig #-}

loadConfig :: MonadIO m => FilePath -> m Config
loadConfig path = liftIO $ loadYamlSettings [path] [] useEnv

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    cfgServerPort     <- o .:? "serverPort"       .!= 8085
    cfgServerTcpPort  <- o .:? "serverTcpPort"    .!= 8667
    cfgServerHostname <- o .:? "serverHostname"   .!= "0.0.0.0"
    pure Config{..}
