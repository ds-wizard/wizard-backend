module Shared.Service.Settings.SettingsSubmissionUtil where

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Shared.Model.Settings.Settings

definitionRequest :: SettingsSubmissionDefinition -> Maybe SettingsSubmissionServiceRequest
definitionRequest definition =
  case definition.target of
    HttpSubmissionTarget {request = request} -> Just request
    ZenodoSubmissionTarget {} -> Nothing

definitionProps :: SettingsSubmissionDefinition -> [String]
definitionProps definition =
  case definition.target of
    HttpSubmissionTarget {props = props} -> props
    ZenodoSubmissionTarget {} -> []

mapDefinitionRequest :: (SettingsSubmissionServiceRequest -> SettingsSubmissionServiceRequest) -> SettingsSubmissionDefinition -> SettingsSubmissionDefinition
mapDefinitionRequest f definition =
  case definition.target of
    target@HttpSubmissionTarget {request = request} -> definition {target = target {request = f request}}
    ZenodoSubmissionTarget {} -> definition

withDefinitionHeaders :: M.Map String String -> SettingsSubmissionDefinition -> SettingsSubmissionDefinition
withDefinitionHeaders headers = mapDefinitionRequest (\request -> request {headers = headers})

findDefinition :: String -> SettingsSubmission -> Maybe SettingsSubmissionDefinition
findDefinition definitionId settings = L.find ((== definitionId) . (.sId)) settings.definitions

isExportOf :: String -> String -> String -> SettingsSubmissionExport -> Bool
isExportOf templateId templateVersion formatName export = export.id == templateId && export.version == templateVersion && export.formatName == formatName
