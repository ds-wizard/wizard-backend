module Shared.Model.OAuth.OAuthSession where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

-- A started OAuth authorization; it is consumed by the callback (the code verifier is stored encrypted). The workspace
-- resolves the settings of the provider; a submission attempted without a connection starts it with the submission to
-- create once connected (project and definition) and where to return
data OAuthSession = OAuthSession
  { state :: String
  , userUuid :: U.UUID
  , provider :: String
  , workspaceUuid :: Maybe U.UUID
  , codeVerifier :: String
  , returnUrl :: Maybe String
  , projectUuid :: Maybe U.UUID
  , definitionId :: Maybe String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
