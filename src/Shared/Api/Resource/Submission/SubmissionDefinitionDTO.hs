module Shared.Api.Resource.Submission.SubmissionDefinitionDTO where

import qualified Data.UUID as U
import GHC.Generics

data SubmissionDefinitionDTO = SubmissionDefinitionDTO
  { sId :: String
  , name :: String
  , description :: String
  , targetType :: String
  , exports :: [SubmissionDefinitionExportDTO]
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
