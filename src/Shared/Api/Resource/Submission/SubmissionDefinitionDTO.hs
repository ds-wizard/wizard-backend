module Shared.Api.Resource.Submission.SubmissionDefinitionDTO where

import qualified Data.UUID as U
import GHC.Generics

data SubmissionDefinitionDTO = SubmissionDefinitionDTO
  { sId :: String
  , name :: String
  , description :: String
  , targetType :: String
  , exports :: [SubmissionDefinitionExportDTO]
  , oauthProvider :: Maybe String
  -- ^ the key of the OAuth provider the target needs a connection to
  , requiresConnection :: Bool
  -- ^ the current user is not connected to it (or with older permissions)
  , connectOnSubmit :: Bool
  -- ^ submitting without the connection starts it and submits once connected
  }
  deriving (Show, Eq, Generic)

data SubmissionDefinitionExportDTO = SubmissionDefinitionExportDTO
  { documentTemplateUuid :: U.UUID
  , documentTemplateName :: String
  , documentTemplateId :: String
  , documentTemplateVersion :: String
  , formatUuid :: U.UUID
  , formatName :: String
  }
  deriving (Show, Eq, Generic)
