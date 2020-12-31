module Tirakatar.App.Monad.Front(
    MonadFront
  , MonadFrontBase(..)
  , MonadFrontAuth(..)
  -- * Helpers
  , getAuthInfo
  , getLoginD
  , requestBroadcast
  -- * Reexports
  , Text
  , MonadJSM
  , traverse_
  , module Tirakatar.App.Monad.Prim
  , module Tirakatar.App.Monad.Base
  , module Reflex.Dom
  , module Reflex.Dom.Retractable.Class
  , module Control.Monad
  , module Tirakatar.App.Monad.Client
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Functor.Misc (Const2(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Network.Socket (SockAddr)
import Language.Javascript.JSaddle hiding ((!!))
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss, textInput)
import Reflex.Dom.Retractable.Class
import Reflex.ExternalRef

import Tirakatar.Text
import Tirakatar.App.Alert
import Tirakatar.App.Monad.Async
import Tirakatar.App.Monad.Base
import Tirakatar.App.Monad.Client
import Tirakatar.App.Monad.Prim
import Tirakatar.App.Monad.Storage
import Tirakatar.App.Native
import Tirakatar.App.Settings
import Tirakatar.App.Status.Types
import Tirakatar.App.Util

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Authorized context. Has access to storage and indexer's functionality
type MonadFront t m = (
    MonadFrontAuth t m
  , MonadStorage t m
  )

class MonadFrontBase t m => MonadFrontAuth t m | m -> t where
  -- | Internal method.
  getStatusUpdRef :: m (ExternalRef t StatusUpdate)
  -- | Get authed info
  getAuthInfoRef :: m (ExternalRef t AuthInfo)

-- | Get the login. Convenience function
getLoginD :: MonadFrontAuth t m => m (Dynamic t Text)
getLoginD = (fmap . fmap) _authInfo'login . externalRefDynamic =<< getAuthInfoRef
{-# INLINE getLoginD #-}

-- | Get auth info. Not a Maybe since this is authorized context
getAuthInfo :: MonadFrontAuth t m => m (Dynamic t AuthInfo)
getAuthInfo = externalRefDynamic =<< getAuthInfoRef

-- | Send the same requests to all URLs
requestBroadcast :: MonadFrontAuth t m => Event t () -> m (Event t ())
requestBroadcast reqE = pure never

randomElem :: MonadIO m => [a] -> m (Maybe a)
randomElem xs = case xs of
  [] -> pure Nothing
  _ -> do
    i <- liftIO $ randomRIO (0, length xs - 1)
    pure $ Just $ xs!!i
