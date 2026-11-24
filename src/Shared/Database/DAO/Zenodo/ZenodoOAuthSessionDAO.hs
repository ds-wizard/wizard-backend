module Shared.Database.DAO.Zenodo.ZenodoOAuthSessionDAO where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Zenodo.ZenodoOAuthSession ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Zenodo.ZenodoOAuthSession

insertZenodoOAuthSession :: WizardRequestContextC s m => ZenodoOAuthSession -> m Int64
insertZenodoOAuthSession session = do
  let sql = "INSERT INTO zenodo_oauth_session (state, user_uuid, repository, code_verifier, tenant_uuid, created_at) VALUES (?, ?, ?, ?, ?, ?)"
  let params = toRow session
  logInsertAndUpdate sql params
  runDB (\conn -> execute conn sql params)

-- A session can be used only once (it is deleted when it is taken)
takeZenodoOAuthSession :: WizardRequestContextC s m => String -> m (Maybe ZenodoOAuthSession)
takeZenodoOAuthSession state = do
  let sql = "DELETE FROM zenodo_oauth_session WHERE state = ? RETURNING state, user_uuid, repository, code_verifier, tenant_uuid, created_at"
  let params = [toField state]
  logQuery sql params
  listToMaybe <$> runDB (\conn -> query conn sql params)

deleteExpiredZenodoOAuthSessions :: WizardRequestContextC s m => m Int64
deleteExpiredZenodoOAuthSessions = do
  let sql = "DELETE FROM zenodo_oauth_session WHERE created_at < now() - interval '1 hour'"
  runDB (`execute_` sql)

deleteZenodoOAuthSessions :: WizardRequestContextC s m => m Int64
deleteZenodoOAuthSessions = createDeleteEntitiesFn "zenodo_oauth_session"
