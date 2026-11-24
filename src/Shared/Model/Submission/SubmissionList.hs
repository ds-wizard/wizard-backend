module Shared.Model.Submission.SubmissionList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Document.Document
import Shared.Model.Submission.Submission
import Shared.Model.User.UserSuggestion

data SubmissionList = SubmissionList
  { uuid :: U.UUID
  , state :: SubmissionState
  , location :: Maybe String
  , returnedData :: Maybe String
  , definitionId :: String
  , definitionName :: Maybe String
  , targetType :: Maybe String
  , projectUuid :: U.UUID
  , externalId :: Maybe String
  , documents :: [SubmissionDocumentList]
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data SubmissionDocumentList = SubmissionDocumentList
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  }
  deriving (Show, Eq, Generic)
