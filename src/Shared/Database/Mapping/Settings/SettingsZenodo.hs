module Shared.Database.Mapping.Settings.SettingsZenodo where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Settings.Settings

instance ToRow SettingsZenodo where
  toRow SettingsZenodo {..} =
    [ toField sandbox.enabled
    , toField sandbox.clientId
    , toField sandbox.clientSecret
    , toField production.enabled
    , toField production.clientId
    , toField production.clientSecret
    ]

instance FromRow SettingsZenodo where
  fromRow = do
    sandbox <- SettingsZenodoRepository <$> field <*> field <*> field
    production <- SettingsZenodoRepository <$> field <*> field <*> field
    return SettingsZenodo {..}
