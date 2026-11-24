module Shared.Model.OAuth.UserOAuthCredential where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

-- The tokens of a user for an OAuth provider (by its key); they are stored encrypted (see OAuthService)
data UserOAuthCredential = UserOAuthCredential
  { userUuid :: U.UUID
  , provider :: String
  , accessToken :: String
  , refreshToken :: Maybe String
  , scope :: String
  , expiresAt :: Maybe UTCTime
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
