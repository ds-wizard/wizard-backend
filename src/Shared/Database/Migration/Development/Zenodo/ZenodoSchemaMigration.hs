module Shared.Database.Migration.Development.Zenodo.ZenodoSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Zenodo) drop tables"
  let sql =
        "DROP TABLE IF EXISTS zenodo_oauth_session CASCADE; \
        \DROP TABLE IF EXISTS user_zenodo_credential CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Zenodo) create tables"
  let sql =
        "CREATE TABLE user_zenodo_credential \
        \( \
        \    user_uuid     uuid        NOT NULL, \
        \    repository    varchar     NOT NULL, \
        \    access_token  varchar     NOT NULL, \
        \    refresh_token varchar, \
        \    scope         varchar     NOT NULL, \
        \    expires_at    timestamptz, \
        \    tenant_uuid   uuid        NOT NULL, \
        \    created_at    timestamptz NOT NULL, \
        \    updated_at    timestamptz NOT NULL, \
        \    CONSTRAINT user_zenodo_credential_pk PRIMARY KEY (user_uuid, repository), \
        \    CONSTRAINT user_zenodo_credential_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_zenodo_credential_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE TABLE zenodo_oauth_session \
        \( \
        \    state         varchar     NOT NULL, \
        \    user_uuid     uuid        NOT NULL, \
        \    repository    varchar     NOT NULL, \
        \    code_verifier varchar     NOT NULL, \
        \    tenant_uuid   uuid        NOT NULL, \
        \    created_at    timestamptz NOT NULL, \
        \    CONSTRAINT zenodo_oauth_session_pk PRIMARY KEY (state), \
        \    CONSTRAINT zenodo_oauth_session_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT zenodo_oauth_session_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
