module Shared.Model.Zenodo.ZenodoRepository where

import Shared.Model.Settings.Settings

zenodoBaseUrl :: ZenodoRepository -> String
zenodoBaseUrl SandboxZenodoRepository = "https://sandbox.zenodo.org"
zenodoBaseUrl ProductionZenodoRepository = "https://zenodo.org"

zenodoRepositorySettings :: SettingsZenodo -> ZenodoRepository -> SettingsZenodoRepository
zenodoRepositorySettings settings SandboxZenodoRepository = settings.sandbox
zenodoRepositorySettings settings ProductionZenodoRepository = settings.production

-- The repositories the users can connect to (enabled with a client configured)
availableZenodoRepositories :: SettingsZenodo -> [ZenodoRepository]
availableZenodoRepositories settings = filter isAvailable [minBound .. maxBound]
  where
    isAvailable repository =
      let repositorySettings = zenodoRepositorySettings settings repository
       in repositorySettings.enabled && not (null repositorySettings.clientId) && not (null repositorySettings.clientSecret)

zenodoRequiredScopes :: [String]
zenodoRequiredScopes = ["deposit:write", "deposit:actions"]

readZenodoRepository :: String -> Maybe ZenodoRepository
readZenodoRepository "SandboxZenodoRepository" = Just SandboxZenodoRepository
readZenodoRepository "ProductionZenodoRepository" = Just ProductionZenodoRepository
readZenodoRepository _ = Nothing
