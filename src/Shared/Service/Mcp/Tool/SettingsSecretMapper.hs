module Shared.Service.Mcp.Tool.SettingsSecretMapper where

import qualified Data.Map.Strict as M

import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Service.Settings.SettingsSubmissionUtil
import Shared.Util.Secret

maskSubmission :: SettingsSubmission -> SettingsSubmission
maskSubmission settings = settings {definitions = fmap (mapDefinitionRequest (\request -> request {headers = maskSecrets request.headers})) settings.definitions}

unmaskSubmission :: SettingsSubmission -> SettingsSubmission -> Either AppError SettingsSubmission
unmaskSubmission current settings = (\definitions -> settings {definitions = definitions}) <$> traverse (unmaskDefinition current) settings.definitions

unmaskDefinition :: SettingsSubmission -> SettingsSubmissionDefinition -> Either AppError SettingsSubmissionDefinition
unmaskDefinition current definition =
  case definitionRequest definition of
    Nothing -> Right definition
    Just request ->
      let mStored = findDefinition definition.sId current >>= definitionRequest
          storedHeaders = maybe M.empty (.headers) mStored
          targetChanged = maybe False ((/= request.url) . (.url)) mStored
          label = "request url of the submission definition " ++ definition.sId
       in (`withDefinitionHeaders` definition) <$> unmaskSecretsFor targetChanged label storedHeaders request.headers

maskRegistry :: SettingsRegistry -> SettingsRegistry
maskRegistry settings = settings {apiKey = maskSecret settings.apiKey}

unmaskRegistry :: SettingsRegistry -> SettingsRegistry -> Either AppError SettingsRegistry
unmaskRegistry current settings = Right settings {apiKey = unmaskSecret current.apiKey settings.apiKey}
