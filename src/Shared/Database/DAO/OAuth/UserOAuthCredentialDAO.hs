module Shared.Database.DAO.OAuth.UserOAuthCredentialDAO where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.OAuth.UserOAuthCredential ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.OAuth.UserOAuthCredential

credentialColumns :: String
credentialColumns = "user_uuid, provider, access_token, refresh_token, scope, expires_at, tenant_uuid, created_at, updated_at"

findUserOAuthCredentials :: WizardRequestContextC s m => U.UUID -> m [UserOAuthCredential]
findUserOAuthCredentials userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ "SELECT " ++ credentialColumns ++ " FROM user_oauth_credential WHERE tenant_uuid = ? AND user_uuid = ?"
  let params = [toField tenantUuid, toField userUuid]
  logQuery sql params
  runDB (\conn -> query conn sql params)

findUserOAuthCredential :: WizardRequestContextC s m => U.UUID -> String -> m (Maybe UserOAuthCredential)
findUserOAuthCredential userUuid provider = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ "SELECT " ++ credentialColumns ++ " FROM user_oauth_credential WHERE tenant_uuid = ? AND user_uuid = ? AND provider = ?"
  let params = [toField tenantUuid, toField userUuid, toField provider]
  logQuery sql params
  listToMaybe <$> runDB (\conn -> query conn sql params)

saveUserOAuthCredential :: WizardRequestContextC s m => UserOAuthCredential -> m Int64
saveUserOAuthCredential credential = do
  let sql =
        fromString $
          "INSERT INTO user_oauth_credential ("
            ++ credentialColumns
            ++ ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
               \ON CONFLICT (user_uuid, provider) DO UPDATE SET access_token = EXCLUDED.access_token, refresh_token = EXCLUDED.refresh_token, \
               \scope = EXCLUDED.scope, expires_at = EXCLUDED.expires_at, updated_at = EXCLUDED.updated_at"
  let params = toRow credential
  logInsertAndUpdate sql params
  runDB (\conn -> execute conn sql params)

deleteUserOAuthCredential :: WizardRequestContextC s m => U.UUID -> String -> m ()
deleteUserOAuthCredential userUuid provider = do
  tenantUuid <- asks (.tenantUuid')
  let sql = "DELETE FROM user_oauth_credential WHERE tenant_uuid = ? AND user_uuid = ? AND provider = ?"
  let params = [toField tenantUuid, toField userUuid, toField provider]
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

deleteUserOAuthCredentials :: WizardRequestContextC s m => m Int64
deleteUserOAuthCredentials = createDeleteEntitiesFn "user_oauth_credential"
