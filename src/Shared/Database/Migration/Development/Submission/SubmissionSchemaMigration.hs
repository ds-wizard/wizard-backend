module Shared.Database.Migration.Development.Submission.SubmissionSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) drop tables"
  let sql =
        "DROP FUNCTION IF EXISTS create_submission_command_on_document_finished CASCADE; \
        \DROP TABLE IF EXISTS submission_document CASCADE; \
        \DROP TABLE IF EXISTS submission CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) create table"
  let sql =
        "CREATE TABLE submission \
        \( \
        \    uuid            uuid        NOT NULL, \
        \    state           varchar     NOT NULL, \
        \    location        varchar, \
        \    returned_data   varchar, \
        \    definition_id   varchar     NOT NULL, \
        \    created_by      uuid, \
        \    created_at      timestamptz, \
        \    updated_at      timestamptz NOT NULL, \
        \    tenant_uuid     uuid        NOT NULL, \
        \    project_uuid    uuid        NOT NULL, \
        \    external_id     varchar, \
        \    target_base_url varchar, \
        \    CONSTRAINT submission_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT submission_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT submission_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX submission_project_uuid_index ON submission (project_uuid, tenant_uuid); \
        \ \
        \CREATE TABLE submission_document \
        \( \
        \    submission_uuid uuid    NOT NULL, \
        \    document_uuid   uuid    NOT NULL, \
        \    position        integer NOT NULL, \
        \    tenant_uuid     uuid    NOT NULL, \
        \    CONSTRAINT submission_document_pk PRIMARY KEY (submission_uuid, document_uuid), \
        \    CONSTRAINT submission_document_submission_uuid_fk FOREIGN KEY (submission_uuid) REFERENCES submission (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT submission_document_document_uuid_fk FOREIGN KEY (document_uuid) REFERENCES document (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT submission_document_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX submission_document_document_uuid_index ON submission_document (document_uuid); \
        \ \
        \CREATE OR REPLACE FUNCTION create_submission_command_on_document_finished() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \DECLARE \
        \    sub record; \
        \BEGIN \
        \    FOR sub IN SELECT submission.uuid, submission.tenant_uuid, submission.created_by \
        \               FROM submission_document \
        \               JOIN submission ON submission.uuid = submission_document.submission_uuid \
        \               WHERE submission_document.document_uuid = NEW.uuid \
        \                 AND submission.state = 'InProgressSubmissionState' \
        \    LOOP \
        \        INSERT INTO persistent_command (uuid, state, component, function, body, last_error_message, attempts, max_attempts, tenant_uuid, created_by, created_at, updated_at, last_trace_uuid) \
        \        VALUES (gen_random_uuid(), 'NewPersistentCommandState', 'submission', 'submit', jsonb_build_object('uuid', sub.uuid), NULL, 0, 3, sub.tenant_uuid, sub.created_by, now(), now(), NULL); \
        \        PERFORM pg_notify('persistent_command_channel', ''); \
        \    END LOOP; \
        \    RETURN NEW; \
        \END; \
        \$$ LANGUAGE plpgsql; \
        \ \
        \CREATE TRIGGER trg_document_after_state_update_submission \
        \    AFTER UPDATE OF state \
        \    ON document \
        \    FOR EACH ROW \
        \    WHEN (OLD.state IS DISTINCT FROM NEW.state AND NEW.state IN ('DoneDocumentState', 'ErrorDocumentState')) \
        \EXECUTE FUNCTION create_submission_command_on_document_finished();"
  let action conn = execute_ conn sql
  runDB action
