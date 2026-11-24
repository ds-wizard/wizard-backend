module Shared.Model.Submission.SubmissionResult where

import GHC.Generics

-- The outcome of sending the documents of a submission to its target
data SubmissionResult = SubmissionResult
  { location :: Maybe String
  , returnedData :: Maybe String
  , externalId :: Maybe String
  , targetBaseUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)
