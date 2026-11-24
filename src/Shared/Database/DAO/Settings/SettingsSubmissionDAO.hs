module Shared.Database.DAO.Settings.SettingsSubmissionDAO where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsSubmission ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_submission"

definitionTable :: String
definitionTable = "settings_submission_definition"

exportTable :: String
exportTable = "settings_submission_definition_export"

childTables :: [String]
childTables = [exportTable, definitionTable]

definitionColumns :: [String]
definitionColumns = ["id", "name", "description", "target", "connect_on_submit"]

exportColumns :: [String]
exportColumns = ["id", "version", "format_name", "madmp_metadata"]

findSettingsSubmission :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m (Maybe SettingsSubmission)
findSettingsSubmission tenantUuid mWorkspaceUuid = do
  let key = scopedKey table tenantUuid mWorkspaceUuid
  mSettings <- findSettingsRow ["enabled"] key
  case mSettings of
    Nothing -> return Nothing
    Just settings -> do
      definitions <- findSettingsChildRows definitionColumns (childKey definitionTable key)
      exports <- groupByDefinitionId <$> findSettingsRows ("definition_id" : exportColumns) "ORDER BY definition_id, position" (childKey exportTable key)
      return . Just $ settings {definitions = fmap (withExports exports) definitions}

withExports :: M.Map String [SettingsSubmissionExport] -> SettingsSubmissionDefinition -> SettingsSubmissionDefinition
withExports exports definition = definition {exports = M.findWithDefault [] definition.sId exports}

groupByDefinitionId :: [Only String :. a] -> M.Map String [a]
groupByDefinitionId rows = M.fromListWith (flip (++)) [(definitionId, [row]) | Only definitionId :. row <- rows]

saveSettingsSubmission :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> SettingsSubmission -> m ()
saveSettingsSubmission tenantUuid mWorkspaceUuid settings = do
  let key = scopedKey table tenantUuid mWorkspaceUuid
  saveSettingsRow ["enabled"] key settings
  forM_ childTables (deleteSettingsRowsByKey . (`childKey` key))
  forM_ (zip [0 :: Int ..] settings.definitions) $ \(position, definition) -> do
    insertSettingsRow ("position" : definitionColumns) (childKey definitionTable key) (toField position : toRow definition)
    let definitionKey = withDefinitionId definition.sId key
    forM_ (zip [0 :: Int ..] definition.exports) $ \(exportPosition, export) ->
      insertSettingsRow ("position" : exportColumns) (childKey exportTable definitionKey) (toField exportPosition : toRow export)

withDefinitionId :: String -> SettingsKey -> SettingsKey
withDefinitionId definitionId key = key {columns = key.columns ++ [("definition_id", toField definitionId)]}
