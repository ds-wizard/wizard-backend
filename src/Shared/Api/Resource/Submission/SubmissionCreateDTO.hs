module Shared.Api.Resource.Submission.SubmissionCreateDTO where

import GHC.Generics

data SubmissionCreateDTO = SubmissionCreateDTO
  { definitionId :: String
  }
  deriving (Show, Eq, Generic)
