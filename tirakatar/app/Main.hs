{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.Text (unpack)
import Tirakatar.App
import Tirakatar.App.Monad.Async
import Tirakatar.App.Run
import Tirakatar.App.Run.Callbacks
import Tirakatar.App.Style
import Tirakatar.App.Version
import GHC.Generics
import Options.Generic

#ifdef ANDROID
import Tirakatar.App.Android.Run()
import Tirakatar.App.Android.Native()
#else
import Tirakatar.App.Desktop.Run()
import Tirakatar.App.Desktop.Native()
#endif

data Options = Options {
  config :: Maybe FilePath <?> "Path to config file"
} deriving (Generic)

instance ParseRecord Options

instance HasVersion where
  version = $embedVersion
  {-# NOINLINE version #-}

main :: IO ()
main = do
  putStrLn $ "Tirakatar version: " <> unpack (makeVersionString version)
  opts <- getRecord "Tirakatar messenger"
  bindSelf $ run $ \cbs -> do
    css <- compileFrontendCss
    mainWidgetWithCss css $ do
      settings :: Settings <- loadSettings $ unHelpful $ config opts
      env <- newEnv settings (runUiCallbacks cbs)
      runEnv cbs env frontend
