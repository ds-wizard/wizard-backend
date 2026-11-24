module Shared.Api.Resource.OAuth.OAuthConnectionDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

-- The connection of the current user to an OAuth provider (by its key, e.g. "zenodo:SandboxZenodoRepository" or
-- "submission:<definition id>"); missingScopes are the permissions an earlier connection lacks
data OAuthConnectionDTO = OAuthConnectionDTO
  { provider :: String
  , name :: String
  , connected :: Bool
  , scope :: Maybe String
  , expiresAt :: Maybe UTCTime
  , missingScopes :: [String]
  }
  deriving (Show, Eq, Generic)

-- With a project and a submission definition, the submission is created once the account is connected; the user then
-- returns to returnUrl (a path of the client)
data OAuthAuthorizeDTO = OAuthAuthorizeDTO
  { provider :: String
  , returnUrl :: Maybe String
  , projectUuid :: Maybe U.UUID
  , definitionId :: Maybe String
  }
  deriving (Show, Eq, Generic)

data OAuthAuthorizationDTO = OAuthAuthorizationDTO
  { url :: String
  }
  deriving (Show, Eq, Generic)

data OAuthCallbackDTO = OAuthCallbackDTO
  { code :: String
  , state :: String
  }
  deriving (Show, Eq, Generic)

data OAuthCallbackResultDTO = OAuthCallbackResultDTO
  { connection :: OAuthConnectionDTO
  , returnUrl :: Maybe String
  , submissionUuid :: Maybe U.UUID
  , submissionError :: Maybe String
  }
  deriving (Show, Eq, Generic)
