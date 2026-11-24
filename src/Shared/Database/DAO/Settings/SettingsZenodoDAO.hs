module Shared.Database.DAO.Settings.SettingsZenodoDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsZenodo ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_zenodo"

columns :: [String]
columns = ["sandbox_enabled", "sandbox_client_id", "sandbox_client_secret", "production_enabled", "production_client_id", "production_client_secret"]

findSettingsZenodo :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsZenodo)
findSettingsZenodo tenantUuid = findSettingsRow columns (orgKey table tenantUuid)

saveSettingsZenodo :: WizardRequestContextC s m => U.UUID -> SettingsZenodo -> m ()
saveSettingsZenodo tenantUuid = saveSettingsRow columns (orgKey table tenantUuid)
