module Shared.Database.DAO.Submission.SubmissionDAO where

import Control.Monad (forM_, void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Submission.Submission ()
import Shared.Database.Mapping.Submission.SubmissionList ()
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Util.String (f')

entityName = "submission"

pageLabel = "submissions"

submissionColumns :: [String]
submissionColumns = ["uuid", "state", "location", "returned_data", "definition_id", "project_uuid", "external_id", "target_base_url", "created_by", "created_at", "updated_at", "tenant_uuid"]

submissionSelect :: String
submissionSelect = f' "SELECT %s FROM submission" [L.intercalate ", " submissionColumns]

findSubmissions :: WizardRequestContextC s m => m [Submission]
findSubmissions = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ submissionSelect ++ " WHERE tenant_uuid = ? ORDER BY created_at"
  let params = [toField tenantUuid]
  logQuery sql params
  runDB (\conn -> query conn sql params)

findSubmissionByUuid :: WizardRequestContextC s m => U.UUID -> m Submission
findSubmissionByUuid uuid = do
  mSubmission <- findSubmissionByUuid' uuid
  maybe (throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND entityName [("uuid", U.toString uuid)]) return mSubmission

findSubmissionByUuid' :: WizardRequestContextC s m => U.UUID -> m (Maybe Submission)
findSubmissionByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ submissionSelect ++ " WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField tenantUuid, toField uuid]
  logQuery sql params
  listToMaybe <$> runDB (\conn -> query conn sql params)

-- Locks the submission row until the end of the transaction, so that two commands finishing the same submission
-- (e.g. two documents done at the same time) cannot both send it.
lockSubmissionByUuid :: WizardRequestContextC s m => U.UUID -> m (Maybe Submission)
lockSubmissionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ submissionSelect ++ " WHERE tenant_uuid = ? AND uuid = ? FOR UPDATE"
  let params = [toField tenantUuid, toField uuid]
  logQuery sql params
  listToMaybe <$> runDB (\conn -> query conn sql params)

findLatestDoneSubmissionWithExternalId :: WizardRequestContextC s m => U.UUID -> String -> String -> U.UUID -> m (Maybe Submission)
findLatestDoneSubmissionWithExternalId projectUuid definitionId targetBaseUrl userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          submissionSelect
            ++ " WHERE tenant_uuid = ? AND project_uuid = ? AND definition_id = ? AND target_base_url = ? AND created_by = ? \
               \ AND state = 'DoneSubmissionState' AND external_id IS NOT NULL \
               \ORDER BY created_at DESC \
               \LIMIT 1"
  let params = [toField tenantUuid, toField projectUuid, toField definitionId, toField targetBaseUrl, toField userUuid]
  logQuery sql params
  listToMaybe <$> runDB (\conn -> query conn sql params)

findSubmissionListsByProjectUuid :: WizardRequestContextC s m => U.UUID -> m [SubmissionList]
findSubmissionListsByProjectUuid projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  findSubmissionLists "submission.project_uuid = ?" [toField tenantUuid, toField projectUuid]

findSubmissionListByUuid :: WizardRequestContextC s m => U.UUID -> m (Maybe SubmissionList)
findSubmissionListByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  listToMaybe <$> findSubmissionLists "submission.uuid = ?" [toField tenantUuid, toField uuid]

findSubmissionLists :: WizardRequestContextC s m => String -> [Action] -> m [SubmissionList]
findSubmissionLists condition params = do
  let sql =
        fromString $
          f'
            "SELECT submission.uuid, \
            \       submission.state, \
            \       submission.location, \
            \       submission.returned_data, \
            \       submission.definition_id, \
            \       submission.project_uuid, \
            \       submission.external_id, \
            \       submission.created_at, \
            \       submission.updated_at, \
            \       definition.name, \
            \       definition.target ->> 'type', \
            \       coalesce((SELECT jsonb_agg(jsonb_build_object('uuid', document.uuid, 'name', document.name, 'state', document.state) ORDER BY submission_document.position) \
            \                 FROM submission_document \
            \                 JOIN document ON document.uuid = submission_document.document_uuid \
            \                 WHERE submission_document.submission_uuid = submission.uuid), '[]'::jsonb), \
            \       user_entity.uuid, \
            \       user_entity.first_name, \
            \       user_entity.last_name, \
            \       gravatar_hash(user_entity.email), \
            \       user_entity.image_url, \
            \       user_entity.affiliation \
            \FROM submission \
            \LEFT JOIN project ON project.uuid = submission.project_uuid AND project.tenant_uuid = submission.tenant_uuid \
            \LEFT JOIN LATERAL (SELECT settings_definition.name, settings_definition.target \
            \                   FROM settings_submission_definition settings_definition \
            \                   WHERE settings_definition.tenant_uuid = submission.tenant_uuid \
            \                     AND settings_definition.id = submission.definition_id \
            \                     AND (settings_definition.workspace_uuid IS NULL OR settings_definition.workspace_uuid = project.workspace_uuid) \
            \                   ORDER BY settings_definition.workspace_uuid NULLS LAST \
            \                   LIMIT 1) definition ON true \
            \LEFT JOIN user_entity ON user_entity.tenant_uuid = submission.tenant_uuid AND user_entity.uuid = submission.created_by \
            \WHERE submission.tenant_uuid = ? AND %s \
            \ORDER BY submission.created_at DESC"
            [condition]
  logQuery sql params
  runDB (\conn -> query conn sql params)

findSubmissionDocuments :: WizardRequestContextC s m => U.UUID -> m [SubmissionDocument]
findSubmissionDocuments submissionUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = "SELECT submission_uuid, position, document_uuid, madmp_metadata, tenant_uuid FROM submission_document WHERE tenant_uuid = ? AND submission_uuid = ? ORDER BY position"
  let params = [toField tenantUuid, toField submissionUuid]
  logQuery sql params
  runDB (\conn -> query conn sql params)

insertSubmission :: WizardRequestContextC s m => Submission -> m Int64
insertSubmission submission = do
  let sql = fromString $ f' "INSERT INTO submission (%s) VALUES (%s)" [L.intercalate ", " submissionColumns, generateQuestionMarks submissionColumns]
  let params = toRow submission
  logInsertAndUpdate sql params
  runDB (\conn -> execute conn sql params)

insertSubmissionDocuments :: WizardRequestContextC s m => [SubmissionDocument] -> m ()
insertSubmissionDocuments submissionDocuments =
  forM_ submissionDocuments $ \submissionDocument -> do
    let sql = "INSERT INTO submission_document (submission_uuid, position, document_uuid, madmp_metadata, tenant_uuid) VALUES (?, ?, ?, ?, ?)"
    let params = toRow submissionDocument
    logInsertAndUpdate sql params
    void $ runDB (\conn -> execute conn sql params)

updateSubmissionByUuid :: WizardRequestContextC s m => Submission -> m Submission
updateSubmissionByUuid sub = do
  now <- liftIO getCurrentTime
  let updatedSub = sub {updatedAt = now} :: Submission
  let sql =
        fromString
          "UPDATE submission \
          \SET state = ?, location = ?, returned_data = ?, external_id = ?, target_base_url = ?, updated_at = ? \
          \WHERE tenant_uuid = ? AND uuid = ?"
  let params =
        [ toField updatedSub.state
        , toField updatedSub.location
        , toField updatedSub.returnedData
        , toField updatedSub.externalId
        , toField updatedSub.targetBaseUrl
        , toField updatedSub.updatedAt
        , toField updatedSub.tenantUuid
        , toField updatedSub.uuid
        ]
  logInsertAndUpdate sql params
  void $ runDB (\conn -> execute conn sql params)
  return updatedSub

-- Runs across all tenants (cron worker); the submissions failed
failTimedOutSubmissions :: WizardRequestContextC s m => Int -> m [Submission]
failTimedOutSubmissions timeoutInMinutes = do
  let sql =
        fromString $
          "UPDATE submission \
          \SET state = 'ErrorSubmissionState', returned_data = 'The submission timed out', updated_at = now() \
          \WHERE state = 'InProgressSubmissionState' AND created_at < now() - make_interval(mins => ?) \
          \RETURNING "
            ++ L.intercalate ", " submissionColumns
  let params = [toField timeoutInMinutes]
  logQuery sql params
  runDB (\conn -> query conn sql params)

deleteSubmissions :: WizardRequestContextC s m => m Int64
deleteSubmissions = createDeleteEntitiesFn entityName
