module Shared.Api.Resource.Zenodo.ZenodoConnectionDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Settings.Settings

data ZenodoConnectionDTO = ZenodoConnectionDTO
  { repository :: ZenodoRepository
  , baseUrl :: String
  , connected :: Bool
  , scope :: Maybe String
  , expiresAt :: Maybe UTCTime
  , missingScopes :: [String]
  }
  deriving (Show, Eq, Generic)

data ZenodoAuthorizeDTO = ZenodoAuthorizeDTO
  { repository :: ZenodoRepository
  }
  deriving (Show, Eq, Generic)

data ZenodoAuthorizationDTO = ZenodoAuthorizationDTO
  { url :: String
  }
  deriving (Show, Eq, Generic)

data ZenodoCallbackDTO = ZenodoCallbackDTO
  { code :: String
  , state :: String
  }
  deriving (Show, Eq, Generic)
