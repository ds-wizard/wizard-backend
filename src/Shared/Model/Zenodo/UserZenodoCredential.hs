module Shared.Model.Zenodo.UserZenodoCredential where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Settings.Settings

-- The tokens are stored encrypted (see ZenodoAuthService)
data UserZenodoCredential = UserZenodoCredential
  { userUuid :: U.UUID
  , repository :: ZenodoRepository
  , accessToken :: String
  , refreshToken :: Maybe String
  , scope :: String
  , expiresAt :: Maybe UTCTime
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
