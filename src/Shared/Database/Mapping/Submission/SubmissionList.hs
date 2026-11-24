module Shared.Database.Mapping.Submission.SubmissionList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Database.Mapping.Submission.Submission ()
import Shared.Database.Mapping.User.UserSuggestion
import Shared.Model.Submission.SubmissionList

instance FromRow SubmissionList where
  fromRow = do
    uuid <- field
    state <- field
    location <- field
    returnedData <- field
    definitionId <- field
    projectUuid <- field
    externalId <- field
    createdAt <- field
    updatedAt <- field
    definitionName <- field
    targetType <- field
    documents <- fieldWith fromJSONField
    createdBy <- fieldUserSuggestion'
    return $ SubmissionList {..}
