module Shared.Service.Submission.SubmissionMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Model.Document.Document
import Shared.Model.Settings.Settings
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Model.User.UserSuggestion

toList :: Submission -> Maybe SettingsSubmissionDefinition -> [Document] -> Maybe UserSuggestion -> SubmissionList
toList submission mDefinition documents createdBy =
  SubmissionList
    { uuid = submission.uuid
    , state = submission.state
    , location = submission.location
    , returnedData = submission.returnedData
    , definitionId = submission.definitionId
    , definitionName = fmap (.name) mDefinition
    , targetType = fmap (targetTypeName . (.target)) mDefinition
    , projectUuid = submission.projectUuid
    , externalId = submission.externalId
    , documents = fmap toDocumentList documents
    , createdBy = createdBy
    , createdAt = submission.createdAt
    , updatedAt = submission.updatedAt
    }

toDocumentList :: Document -> SubmissionDocumentList
toDocumentList document = SubmissionDocumentList {uuid = document.uuid, name = document.name, state = document.state}

targetTypeName :: SettingsSubmissionTarget -> String
targetTypeName HttpSubmissionTarget {} = "HttpSubmissionTarget"
targetTypeName ZenodoSubmissionTarget {} = "ZenodoSubmissionTarget"

fromCreate :: U.UUID -> String -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> Submission
fromCreate uuid definitionId projectUuid tenantUuid createdBy now =
  Submission
    { uuid = uuid
    , state = InProgressSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , definitionId = definitionId
    , projectUuid = projectUuid
    , externalId = Nothing
    , targetBaseUrl = Nothing
    , tenantUuid = tenantUuid
    , createdBy = createdBy
    , createdAt = now
    , updatedAt = now
    }

-- The documents with the maDMP flags of their exports, in the order of the exports
toSubmissionDocuments :: U.UUID -> U.UUID -> [(U.UUID, Bool)] -> [SubmissionDocument]
toSubmissionDocuments submissionUuid tenantUuid documents =
  [ SubmissionDocument {submissionUuid = submissionUuid, position = position, documentUuid = Just documentUuid, madmpMetadata = madmpMetadata, tenantUuid = tenantUuid}
  | (position, (documentUuid, madmpMetadata)) <- zip [0 ..] documents
  ]
