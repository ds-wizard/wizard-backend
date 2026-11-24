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

-- A document of a submission at the position of its export; the export's maDMP flag is kept, so that editing the
-- definition later does not change how the document is sent (a deleted document leaves the row without a document)
data SubmissionDocument = SubmissionDocument
  { submissionUuid :: U.UUID
  , position :: Int
  , documentUuid :: Maybe U.UUID
  , madmpMetadata :: Bool
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
