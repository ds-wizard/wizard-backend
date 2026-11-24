module Shared.Database.DAO.OAuth.OAuthSessionDAO where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.OAuth.OAuthSession ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.OAuth.OAuthSession

insertOAuthSession :: WizardRequestContextC s m => OAuthSession -> m Int64
insertOAuthSession session = do
  let sql =
        "INSERT INTO oauth_session (state, user_uuid, provider, workspace_uuid, code_verifier, return_url, project_uuid, definition_id, tenant_uuid, created_at) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  let params = toRow session
  logInsertAndUpdate sql params
  runDB (\conn -> execute conn sql params)

-- A session can be used only once (it is deleted when it is taken)
takeOAuthSession :: WizardRequestContextC s m => String -> m (Maybe OAuthSession)
takeOAuthSession state = do
  let sql =
        "DELETE FROM oauth_session WHERE state = ? \
        \RETURNING state, user_uuid, provider, workspace_uuid, code_verifier, return_url, project_uuid, definition_id, tenant_uuid, created_at"
  let params = [toField state]
  logQuery sql params
  listToMaybe <$> runDB (\conn -> query conn sql params)

deleteExpiredOAuthSessions :: WizardRequestContextC s m => m Int64
deleteExpiredOAuthSessions = do
  let sql = "DELETE FROM oauth_session WHERE created_at < now() - interval '1 hour'"
  runDB (`execute_` sql)

deleteOAuthSessions :: WizardRequestContextC s m => m Int64
deleteOAuthSessions = createDeleteEntitiesFn "oauth_session"
