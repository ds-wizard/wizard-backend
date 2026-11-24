module Shared.Model.Zenodo.ZenodoOAuthSession where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Settings.Settings

-- A started OAuth authorization; it is consumed by the callback (the code verifier is stored encrypted)
data ZenodoOAuthSession = ZenodoOAuthSession
  { state :: String
  , userUuid :: U.UUID
  , repository :: ZenodoRepository
  , codeVerifier :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
