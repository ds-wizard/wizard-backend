module Shared.Api.Resource.Submission.SubmissionDefinitionJM where

import Data.Aeson

import Shared.Api.Resource.Submission.SubmissionDefinitionDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON SubmissionDefinitionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionDefinitionDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SubmissionDefinitionDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SubmissionDefinitionExportDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionDefinitionExportDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SubmissionDefinitionExportDTO where
  jsonSchema = genericJsonSchema jsonOptions
