module Shared.Model.Submission.Submission where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data SubmissionState
  = InProgressSubmissionState
  | DoneSubmissionState
  | ErrorSubmissionState
  deriving (Show, Eq, Generic, Read)

data Submission = Submission
  { uuid :: U.UUID
  , state :: SubmissionState
  , location :: Maybe String
  , returnedData :: Maybe String
  , definitionId :: String
  , projectUuid :: U.UUID
  , externalId :: Maybe String
  , targetBaseUrl :: Maybe String
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data SubmissionDocument = SubmissionDocument
  { submissionUuid :: U.UUID
  , documentUuid :: U.UUID
  , position :: Int
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
