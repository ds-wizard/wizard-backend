module Shared.Database.DAO.Zenodo.UserZenodoCredentialDAO where

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
import Shared.Database.Mapping.Zenodo.UserZenodoCredential ()
import Shared.Database.Mapping.Zenodo.ZenodoRepository ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings
import Shared.Model.Zenodo.UserZenodoCredential

credentialColumns :: String
credentialColumns = "user_uuid, repository, access_token, refresh_token, scope, expires_at, tenant_uuid, created_at, updated_at"

findUserZenodoCredentials :: WizardRequestContextC s m => U.UUID -> m [UserZenodoCredential]
findUserZenodoCredentials userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ "SELECT " ++ credentialColumns ++ " FROM user_zenodo_credential WHERE tenant_uuid = ? AND user_uuid = ?"
  let params = [toField tenantUuid, toField userUuid]
  logQuery sql params
  runDB (\conn -> query conn sql params)

findUserZenodoCredential :: WizardRequestContextC s m => U.UUID -> ZenodoRepository -> m (Maybe UserZenodoCredential)
findUserZenodoCredential userUuid repository = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ "SELECT " ++ credentialColumns ++ " FROM user_zenodo_credential WHERE tenant_uuid = ? AND user_uuid = ? AND repository = ?"
  let params = [toField tenantUuid, toField userUuid, toField repository]
  logQuery sql params
  listToMaybe <$> runDB (\conn -> query conn sql params)

saveUserZenodoCredential :: WizardRequestContextC s m => UserZenodoCredential -> m Int64
saveUserZenodoCredential credential = do
  let sql =
        fromString $
          "INSERT INTO user_zenodo_credential ("
            ++ credentialColumns
            ++ ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
               \ON CONFLICT (user_uuid, repository) DO UPDATE SET access_token = EXCLUDED.access_token, refresh_token = EXCLUDED.refresh_token, \
               \scope = EXCLUDED.scope, expires_at = EXCLUDED.expires_at, updated_at = EXCLUDED.updated_at"
  let params = toRow credential
  logInsertAndUpdate sql params
  runDB (\conn -> execute conn sql params)

deleteUserZenodoCredential :: WizardRequestContextC s m => U.UUID -> ZenodoRepository -> m ()
deleteUserZenodoCredential userUuid repository = do
  tenantUuid <- asks (.tenantUuid')
  let sql = "DELETE FROM user_zenodo_credential WHERE tenant_uuid = ? AND user_uuid = ? AND repository = ?"
  let params = [toField tenantUuid, toField userUuid, toField repository]
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

deleteUserZenodoCredentials :: WizardRequestContextC s m => m Int64
deleteUserZenodoCredentials = createDeleteEntitiesFn "user_zenodo_credential"
