module Main where

import Control.Monad.Logger
import Data.Text (Text, pack)
import Options.Applicative

import Tirakatar.Server

import qualified Data.Text.IO as T

data Options = Options {
  -- | Which command to execute
  optsCommand :: Command
}

type ServerUrl = Text

data Command = CommandListen FilePath

options :: Parser Options
options = Options
  <$> subparser (
       command "listen" (info (listenCmd <**> helper) $ progDesc "Start server")
  )
  where
    listenCmd = CommandListen
      <$> strArgument (
          metavar "CONFIG_PATH"
        )

main :: IO ()
main = do
    startServer =<< execParser opts
    where
      opts = info (options <**> helper)
        ( fullDesc
       <> progDesc "Starts Tirakatar messenger server"
       <> header "tirakatar-server - Server for Tirakatar messenger" )

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
    CommandListen cfgPath -> do
      T.putStrLn $ pack "Server starting"
      cfg <- loadConfig cfgPath
      env <- runStdoutLoggingT $ newServerEnv cfg
      runStdoutLoggingT $ app env
