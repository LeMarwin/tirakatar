-- Frontend-wide types
module Tirakatar.App.Types
  (
    Password
  , AuthInfo(..)
  -- * Export lenses
  , authInfo'storage
  , authInfo'eciesPubKey
  , authInfo'login
  , authInfo'isUpdate
  , authInfo'isPlain
  ) where

import Control.Lens
import Data.Text (Text)
import Tirakatar.Crypto.ECIES
import Tirakatar.Types.Storage

data AuthInfo = AuthInfo {
  _authInfo'storage     :: !TirStorage
, _authInfo'eciesPubKey :: !ECIESPubKey
, _authInfo'login       :: !Text
, _authInfo'isUpdate    :: !Bool
  -- ^ This field indicates whether the widget should be redrawn in 'liftAuth'.
  -- 'False' means that the value obtained as a result of updating the previous 'AuthInfo',
  -- 'True' means that the value was newly created or loaded from the storage file at startup.
, _authInfo'isPlain     :: !Bool
  -- ^ this field indicates if the storage is encrypted with empty string, aka not encrypted
} deriving (Eq)

makeLenses ''AuthInfo
