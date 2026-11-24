module WizardServer.Database.Migration.Production.Migration_5_2_0.Migration (
  definition,
) where

import Control.Monad.Logger
import Data.Pool (Pool)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple
import Text.RawString.QQ

import WizardServer.Database.Migration.Production.Migration_5_0_0.Migration (runSql)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5002000, mmName = "Submission definitions", mmDescription = "Replace submission services with named submission definitions, submit projects and connect Zenodo"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createSubmissionDefinitionTables dbPool
  migrateSubmissionServices dbPool
  dropSubmissionServiceTables dbPool
  migrateSubmissionsToProjects dbPool
  createSubmissionTrigger dbPool
  createZenodoTables dbPool
  return Nothing

createSubmissionDefinitionTables :: Pool Connection -> LoggingT IO ()
createSubmissionDefinitionTables dbPool =
  runSql
    dbPool
    [r|
CREATE TABLE settings_submission_definition
(
    tenant_uuid    uuid    NOT NULL,
    workspace_uuid uuid,
    id             varchar NOT NULL,
    position       integer NOT NULL,
    name           varchar NOT NULL,
    description    varchar NOT NULL,
    target            jsonb   NOT NULL,
    connect_on_submit boolean NOT NULL DEFAULT true,
    CONSTRAINT settings_submission_definition_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid, id),
    CONSTRAINT settings_submission_definition_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE,
    CONSTRAINT settings_submission_definition_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE
);

CREATE TABLE settings_submission_definition_export
(
    tenant_uuid    uuid    NOT NULL,
    workspace_uuid uuid,
    definition_id  varchar NOT NULL,
    position       integer NOT NULL,
    id             varchar NOT NULL,
    version        varchar NOT NULL,
    format_name    varchar NOT NULL,
    madmp_metadata boolean NOT NULL,
    CONSTRAINT settings_submission_definition_export_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid, definition_id, position),
    CONSTRAINT settings_submission_definition_export_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE,
    CONSTRAINT settings_submission_definition_export_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE
);
|]

-- Every (service, supported format) pair becomes one definition with a single export. The first pair of a service keeps
-- the service id (so existing submissions and user props keep pointing to it), the others get "<id>-<n>".
migrateSubmissionServices :: Pool Connection -> LoggingT IO ()
migrateSubmissionServices dbPool =
  runSql
    dbPool
    [r|
CREATE TEMPORARY TABLE submission_service_format_migration ON COMMIT DROP AS
SELECT service.tenant_uuid,
       service.workspace_uuid,
       service.id                                                                     AS service_id,
       CASE WHEN format.rn IS NULL OR format.rn = 1 THEN service.id ELSE service.id || '-' || format.rn END AS definition_id,
       CASE WHEN format.cnt > 1 THEN service.name || ' - ' || format.format_name ELSE service.name END     AS definition_name,
       format.id                                                                      AS template_id,
       format.version                                                                 AS template_version,
       format.format_name,
       coalesce(format.rn, 1)                                                         AS rn
FROM settings_submission_service service
         LEFT JOIN (SELECT f.*,
                           row_number() OVER (PARTITION BY f.tenant_uuid, f.workspace_uuid, f.service_id ORDER BY f.id, f.version, f.format_name) AS rn,
                           count(*) OVER (PARTITION BY f.tenant_uuid, f.workspace_uuid, f.service_id)                                             AS cnt
                    FROM settings_submission_service_supported_format f) format
                   ON format.tenant_uuid = service.tenant_uuid
                       AND format.workspace_uuid IS NOT DISTINCT FROM service.workspace_uuid
                       AND format.service_id = service.id;

INSERT INTO settings_submission_definition (tenant_uuid, workspace_uuid, id, position, name, description, target)
SELECT m.tenant_uuid,
       m.workspace_uuid,
       m.definition_id,
       row_number() OVER (PARTITION BY m.tenant_uuid, m.workspace_uuid ORDER BY m.service_id, m.rn) - 1,
       m.definition_name,
       service.description,
       jsonb_build_object(
               'type', 'HttpSubmissionTarget',
               'props', to_jsonb(service.props),
               'request', jsonb_build_object(
                       'method', service.request_method,
                       'url', service.request_url,
                       'headers', coalesce((SELECT jsonb_object_agg(header.name, header.value)
                                            FROM settings_submission_service_request_header header
                                            WHERE header.tenant_uuid = service.tenant_uuid
                                              AND header.workspace_uuid IS NOT DISTINCT FROM service.workspace_uuid
                                              AND header.service_id = service.id), '{}'::jsonb),
                       'multipart', jsonb_build_object(
                               'enabled', service.request_multipart_enabled,
                               'fileName', service.request_multipart_file_name)))
FROM submission_service_format_migration m
         JOIN settings_submission_service service
              ON service.tenant_uuid = m.tenant_uuid
                  AND service.workspace_uuid IS NOT DISTINCT FROM m.workspace_uuid
                  AND service.id = m.service_id
ON CONFLICT DO NOTHING;

INSERT INTO settings_submission_definition_export (tenant_uuid, workspace_uuid, definition_id, position, id, version, format_name, madmp_metadata)
SELECT tenant_uuid, workspace_uuid, definition_id, 0, template_id, template_version, format_name, false
FROM submission_service_format_migration
WHERE template_id IS NOT NULL
ON CONFLICT DO NOTHING;

INSERT INTO user_entity_submission_prop (user_uuid, service_id, values, tenant_uuid, created_at, updated_at)
SELECT prop.user_uuid, m.definition_id, prop.values, prop.tenant_uuid, prop.created_at, prop.updated_at
FROM user_entity_submission_prop prop
         JOIN (SELECT DISTINCT tenant_uuid, service_id, definition_id
               FROM submission_service_format_migration
               WHERE definition_id <> service_id) m
              ON m.tenant_uuid = prop.tenant_uuid AND m.service_id = prop.service_id
ON CONFLICT DO NOTHING;
|]

dropSubmissionServiceTables :: Pool Connection -> LoggingT IO ()
dropSubmissionServiceTables dbPool =
  runSql
    dbPool
    [r|
DROP TABLE settings_submission_service_supported_format;
DROP TABLE settings_submission_service_request_header;
DROP TABLE settings_submission_service;
|]

-- A submission belongs to a project now and references its documents through submission_document
migrateSubmissionsToProjects :: Pool Connection -> LoggingT IO ()
migrateSubmissionsToProjects dbPool =
  runSql
    dbPool
    [r|
ALTER TABLE submission RENAME COLUMN service_id TO definition_id;
ALTER TABLE submission
    ADD COLUMN project_uuid    uuid,
    ADD COLUMN external_id     varchar,
    ADD COLUMN target_base_url varchar;

CREATE TABLE submission_document
(
    submission_uuid uuid    NOT NULL,
    document_uuid   uuid    NOT NULL,
    position        integer NOT NULL,
    tenant_uuid     uuid    NOT NULL,
    CONSTRAINT submission_document_pk PRIMARY KEY (submission_uuid, document_uuid),
    CONSTRAINT submission_document_submission_uuid_fk FOREIGN KEY (submission_uuid) REFERENCES submission (uuid) ON DELETE CASCADE,
    CONSTRAINT submission_document_document_uuid_fk FOREIGN KEY (document_uuid) REFERENCES document (uuid) ON DELETE CASCADE,
    CONSTRAINT submission_document_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE
);

CREATE INDEX submission_document_document_uuid_index ON submission_document (document_uuid);

UPDATE submission
SET project_uuid = document.project_uuid
FROM document
WHERE document.uuid = submission.document_uuid;

INSERT INTO submission_document (submission_uuid, document_uuid, position, tenant_uuid)
SELECT uuid, document_uuid, 0, tenant_uuid
FROM submission
WHERE document_uuid IS NOT NULL AND project_uuid IS NOT NULL;

DELETE FROM submission WHERE project_uuid IS NULL;

DROP INDEX IF EXISTS submission_document_uuid_index;
ALTER TABLE submission DROP CONSTRAINT IF EXISTS submission_document_uuid_fk;
ALTER TABLE submission DROP COLUMN document_uuid;
ALTER TABLE submission ALTER COLUMN project_uuid SET NOT NULL;
ALTER TABLE submission ADD CONSTRAINT submission_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE CASCADE;
CREATE INDEX submission_project_uuid_index ON submission (project_uuid, tenant_uuid);
|]

-- When a document a submission waits for is finished, a "submission/submit" command (run as the submitting user) finishes the submission
createSubmissionTrigger :: Pool Connection -> LoggingT IO ()
createSubmissionTrigger dbPool =
  runSql
    dbPool
    [r|
CREATE OR REPLACE FUNCTION create_submission_command_on_document_finished()
    RETURNS TRIGGER AS
$$
DECLARE
    sub record;
BEGIN
    FOR sub IN SELECT submission.uuid, submission.tenant_uuid, submission.created_by
               FROM submission_document
               JOIN submission ON submission.uuid = submission_document.submission_uuid
               WHERE submission_document.document_uuid = NEW.uuid
                 AND submission.state = 'InProgressSubmissionState'
    LOOP
        INSERT INTO persistent_command (uuid, state, component, function, body, last_error_message, attempts, max_attempts, tenant_uuid, created_by, created_at, updated_at, last_trace_uuid)
        VALUES (gen_random_uuid(), 'NewPersistentCommandState', 'submission', 'submit', jsonb_build_object('uuid', sub.uuid), NULL, 0, 3, sub.tenant_uuid, sub.created_by, now(), now(), NULL);
        PERFORM pg_notify('persistent_command_channel', '');
    END LOOP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_document_after_state_update_submission
    AFTER UPDATE OF state
    ON document
    FOR EACH ROW
    WHEN (OLD.state IS DISTINCT FROM NEW.state AND NEW.state IN ('DoneDocumentState', 'ErrorDocumentState'))
EXECUTE FUNCTION create_submission_command_on_document_finished();
|]

createZenodoTables :: Pool Connection -> LoggingT IO ()
createZenodoTables dbPool =
  runSql
    dbPool
    [r|
CREATE TABLE settings_zenodo
(
    tenant_uuid              uuid        NOT NULL,
    sandbox_enabled          boolean     NOT NULL,
    sandbox_client_id        varchar     NOT NULL,
    sandbox_client_secret    varchar     NOT NULL,
    production_enabled       boolean     NOT NULL,
    production_client_id     varchar     NOT NULL,
    production_client_secret varchar     NOT NULL,
    created_at               timestamptz NOT NULL,
    updated_at               timestamptz NOT NULL,
    CONSTRAINT settings_zenodo_pk PRIMARY KEY (tenant_uuid),
    CONSTRAINT settings_zenodo_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE
);

INSERT INTO settings_zenodo (tenant_uuid, sandbox_enabled, sandbox_client_id, sandbox_client_secret, production_enabled, production_client_id, production_client_secret, created_at, updated_at)
SELECT uuid, false, '', '', false, '', '', now(), now()
FROM tenant;

CREATE TABLE user_oauth_credential
(
    user_uuid     uuid        NOT NULL,
    provider      varchar     NOT NULL,
    access_token  varchar     NOT NULL,
    refresh_token varchar,
    scope         varchar     NOT NULL,
    expires_at    timestamptz,
    tenant_uuid   uuid        NOT NULL,
    created_at    timestamptz NOT NULL,
    updated_at    timestamptz NOT NULL,
    CONSTRAINT user_oauth_credential_pk PRIMARY KEY (user_uuid, provider),
    CONSTRAINT user_oauth_credential_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE,
    CONSTRAINT user_oauth_credential_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE
);

CREATE TABLE oauth_session
(
    state          varchar     NOT NULL,
    user_uuid      uuid        NOT NULL,
    provider       varchar     NOT NULL,
    workspace_uuid uuid,
    code_verifier  varchar     NOT NULL,
    return_url     varchar,
    project_uuid   uuid,
    definition_id  varchar,
    tenant_uuid    uuid        NOT NULL,
    created_at     timestamptz NOT NULL,
    CONSTRAINT oauth_session_pk PRIMARY KEY (state),
    CONSTRAINT oauth_session_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE,
    CONSTRAINT oauth_session_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE
);
|]
