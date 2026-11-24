module Shared.Api.Resource.Submission.SubmissionDefinitionSM where

import Data.Swagger

import Shared.Api.Resource.Submission.SubmissionDefinitionDTO
import Shared.Api.Resource.Submission.SubmissionDefinitionJM ()
import Shared.Util.Aeson

instance ToSchema SubmissionDefinitionDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SubmissionDefinitionExportDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)
