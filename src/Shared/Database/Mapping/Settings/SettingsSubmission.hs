module Shared.Database.Mapping.Settings.SettingsSubmission where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Model.Settings.Settings

instance ToRow SettingsSubmission where
  toRow SettingsSubmission {..} = [toField enabled]

instance FromRow SettingsSubmission where
  fromRow = do
    enabled <- field
    let definitions = []
    return SettingsSubmission {..}

instance ToRow SettingsSubmissionDefinition where
  toRow SettingsSubmissionDefinition {..} =
    [ toField sId
    , toField name
    , toField description
    , toJSONField target
    , toField connectOnSubmit
    ]

instance FromRow SettingsSubmissionDefinition where
  fromRow = do
    sId <- field
    name <- field
    description <- field
    target <- fieldWith fromJSONField
    connectOnSubmit <- field
    let exports = []
    return SettingsSubmissionDefinition {..}

instance ToRow SettingsSubmissionExport

instance FromRow SettingsSubmissionExport
