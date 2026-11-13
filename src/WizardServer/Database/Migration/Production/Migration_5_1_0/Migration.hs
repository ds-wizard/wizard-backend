module WizardServer.Database.Migration.Production.Migration_5_1_0.Migration (
  definition,
) where

import Control.Monad.Logger
import Data.Pool (Pool)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

import WizardServer.Database.Migration.Production.Migration_5_0_0.Migration (runSql)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5001000, mmName = "Delete item event", mmDescription = "Add DeleteItemEvent to the project event types"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  runSql dbPool "ALTER TYPE project_event_type ADD VALUE IF NOT EXISTS 'DeleteItemEvent';"
  return Nothing
