{-# LANGUAGE CPP #-}
module Tirakatar.App.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Reflex.Dom.Main (mainWidgetWithCss)

import Control.Monad.IO.Class

import Tirakatar.App.Alert.Handler
import Tirakatar.App.Loading
import Tirakatar.App.Log.Writer
import Tirakatar.App.Monad
import Tirakatar.App.Native
import Tirakatar.App.Password
import Tirakatar.App.Page.Initial
import Tirakatar.App.Elements

frontend :: MonadFrontBase t m => m ()
frontend = do
  logWrite "Frontend started"
  loadingWidget
  askPasswordModal
  alertHandlerWidget
  logWriter =<< fmap fst getLogsTrigger
  logWrite "Entering initial page"
  void $ retractStack (initialPage True) `liftAuth` retractStack startPage

startPage :: MonadFront t m => m ()
startPage = do
  n <- getStorageName
  h2 $ text n
