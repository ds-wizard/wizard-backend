module Shared.Service.Submission.SubmissionService where

import Control.Monad (forM, forM_, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Api.Resource.Submission.SubmissionDefinitionDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Constant.PersistentCommand (submissionComponent, submissionSubmitFunction)
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Submission.Runner
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Automation.AutomationEvent
import Shared.Model.Automation.AutomationHook
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.Trigger.TriggerEntityUuidCommand
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Model.Submission.SubmissionResult
import Shared.Model.User.UserSubmissionPropList
import Shared.Model.OAuth.OAuthProvider
import Shared.S3.Document.DocumentS3
import Shared.Service.Automation.AutomationHookService
import Shared.Service.Common
import Shared.Service.Document.Context.DocumentContextService
import Shared.Service.Document.DocumentAcl
import Shared.Service.Document.DocumentAuditLog
import Shared.Service.Document.DocumentService
import Shared.Service.Document.DocumentUtil
import Shared.Service.DocumentTemplate.WizardDocumentTemplateUtil
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Service.OAuth.OAuthService
import Shared.Service.Settings.SettingsSubmissionUtil
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Service.Submission.SubmissionMapper
import Shared.Service.User.Profile.UserProfileService
import Shared.Service.Zenodo.ZenodoSubmitter
import Shared.Util.JSON
import Shared.Util.Uuid

-- An export of a submission definition resolved to a template and a format usable by the project
data ResolvedExport = ResolvedExport
  { export :: SettingsSubmissionExport
  , template :: DocumentTemplate
  , format :: DocumentTemplateFormatSimple
  }

getAvailableSubmissions :: WizardRequestContextC s m => U.UUID -> m [SubmissionDefinitionDTO]
getAvailableSubmissions projectUuid = do
  project <- findProjectByUuid projectUuid
  checkIfSubmissionIsEnabled project.workspaceUuid
  checkEditPermissionToDoc (Just project.uuid)
  settings <- getEffectiveSettingsSubmission (Just project.workspaceUuid)
  usableTemplates <- findUsableTemplates project
  currentUser <- getCurrentUser
  definitions <- forM settings.definitions $ \definition -> do
    -- A definition whose OAuth provider is not configured (e.g. a disabled Zenodo repository) is not offered
    mConnection <-
      case definitionOAuthProvider definition of
        Nothing -> return (Just Nothing)
        Just provider -> do
          mConfig <- resolveProviderConfig (Just project.workspaceUuid) provider
          case mConfig of
            Nothing -> return Nothing
            Just config -> Just . Just . (,) provider <$> isConnected currentUser.uuid provider config
    case mConnection of
      Nothing -> return Nothing
      Just connection -> do
        mExports <- resolveExports usableTemplates definition
        return $ fmap (toDefinitionDTO definition connection) mExports
  return . catMaybes $ definitions

getSubmissionsForProject :: WizardRequestContextC s m => U.UUID -> m [SubmissionList]
getSubmissionsForProject projectUuid = do
  project <- findProjectByUuid projectUuid
  checkIfSubmissionIsEnabled project.workspaceUuid
  checkViewPermissionToDoc (Just project.uuid)
  findSubmissionListsByProjectUuid project.uuid

getSubmissionForProject :: WizardRequestContextC s m => U.UUID -> U.UUID -> m SubmissionList
getSubmissionForProject projectUuid submissionUuid = do
  project <- findProjectByUuid projectUuid
  checkIfSubmissionIsEnabled project.workspaceUuid
  checkViewPermissionToDoc (Just project.uuid)
  mSubmission <- findSubmissionListByUuid submissionUuid
  case mSubmission of
    Just submission | submission.projectUuid == project.uuid -> return submission
    _ -> throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "submission" [("uuid", U.toString submissionUuid)]

-- Creates the submission, reuses the documents already generated from the current state of the project, queues the
-- missing ones and a command that sends them once they are all done (see SubmissionCommandExecutor)
createSubmission :: WizardRequestContextC s m => U.UUID -> SubmissionCreateDTO -> m SubmissionList
createSubmission projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkIfSubmissionIsEnabled project.workspaceUuid
    checkEditPermissionToDoc (Just project.uuid)
    settings <- getEffectiveSettingsSubmission (Just project.workspaceUuid)
    definition <- maybe (notAvailable reqDto.definitionId) return (findDefinition reqDto.definitionId settings)
    usableTemplates <- findUsableTemplates project
    exports <- resolveExports usableTemplates definition >>= maybe (notAvailable reqDto.definitionId) return
    _ <- checkAutomationHook (ProjectSubmitHook project definition.sId)
    checkTargetPrerequisites project definition
    (ctx, mCutOffEventUuid) <- createProjectDocumentContext project Nothing
    existingDocuments <-
      findDocumentsForCurrentTenantFiltered
        [ ("project_uuid", U.toString project.uuid)
        , ("context_hash", ctx.hash)
        , ("durability", "PersistentDocumentDurability")
        ]
    documents <- forM exports $ \resolved ->
      case findReusableDocument resolved existingDocuments of
        Just document -> return document
        Nothing -> createDocumentFromContext (project.name ++ " - " ++ definition.name) project resolved.template resolved.format ctx mCutOffEventUuid
    currentUser <- getCurrentUser
    submissionUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let submission = fromCreate submissionUuid definition.sId project.uuid project.tenantUuid (Just currentUser.uuid) now
    insertSubmission submission
    insertSubmissionDocuments (toSubmissionDocuments submission.uuid project.tenantUuid (fmap (.uuid) documents))
    commandUuid <- liftIO generateUuid
    let body = encodeJsonToString (TriggerEntityUuidCommand {uuid = submission.uuid})
    insertPersistentCommand (toPersistentCommand commandUuid submissionComponent submissionSubmitFunction body 3 project.tenantUuid (Just currentUser.uuid) now)
    mSubmissionList <- findSubmissionListByUuid submission.uuid
    maybe (throwError . GeneralServerError $ "Submission was not created") return mSubmissionList

-- Called by the submission command (as the user who created the submission) whenever one of its documents is finished
finishSubmission :: WizardRequestContextC s m => U.UUID -> m ()
finishSubmission submissionUuid = do
  mSubmission <- lockSubmissionByUuid submissionUuid
  case mSubmission of
    Just submission | submission.state == InProgressSubmissionState -> do
      documents <- traverse findDocumentByUuid =<< findSubmissionDocumentUuids submission.uuid
      case (documents, L.find ((== ErrorDocumentState) . (.state)) documents) of
        ([], _) -> failSubmission submission documents "The documents of the submission were deleted"
        (_, Just failed) -> failSubmission submission documents ("The document '" ++ failed.name ++ "' could not be generated")
        _
          | any isPendingDocument documents -> return ()
          | otherwise -> sendSubmission submission documents
    _ -> return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
notAvailable :: WizardRequestContextC s m => String -> m a
notAvailable definitionId = throwError . UserError $ _ERROR_SERVICE_SUBMISSION__DEFINITION_NOT_AVAILABLE definitionId

checkIfSubmissionIsEnabled :: WizardRequestContextC s m => U.UUID -> m ()
checkIfSubmissionIsEnabled workspaceUuid = checkIfTenantFeatureIsEnabled "Submission" (getEffectiveSettingsSubmission (Just workspaceUuid)) (.enabled)

-- A target with an OAuth provider (Zenodo, an HTTP target with OAuth) needs the provider configured and the account of
-- the user connected to it
checkTargetPrerequisites :: WizardRequestContextC s m => Project -> SettingsSubmissionDefinition -> m ()
checkTargetPrerequisites project definition =
  case definitionOAuthProvider definition of
    Nothing -> return ()
    Just provider -> do
      mConfig <- resolveProviderConfig (Just project.workspaceUuid) provider
      config <- maybe (notAvailable definition.sId) return mConfig
      currentUser <- getCurrentUser
      connected <- isConnected currentUser.uuid provider config
      unless connected (throwError . UserError $ _ERROR_SERVICE_SUBMISSION__NOT_CONNECTED config.name)

findUsableTemplates :: WizardRequestContextC s m => Project -> m [DocumentTemplate]
findUsableTemplates project = do
  templates <- findDocumentTemplatesInWorkspace (Just project.workspaceUuid)
  pkg <- findPackageByUuid project.knowledgeModelPackageUuid
  return $ filterDocumentTemplates (Just . createCoordinate $ pkg) templates

-- Nothing when the definition has no exports or one of them is not usable for the project (unknown template or format,
-- template not compatible with the knowledge model of the project)
resolveExports :: WizardRequestContextC s m => [DocumentTemplate] -> SettingsSubmissionDefinition -> m (Maybe [ResolvedExport])
resolveExports _ SettingsSubmissionDefinition {exports = []} = return Nothing
resolveExports usableTemplates definition = do
  resolved <- forM definition.exports $ \export ->
    case L.find (\t -> t.id == export.id && t.version == export.version) usableTemplates of
      Nothing -> return Nothing
      Just template -> do
        formats <- findDocumentTemplateFormats template.uuid
        return $ case L.find ((== export.formatName) . (.name)) formats of
          Just format -> Just ResolvedExport {export = export, template = template, format = DocumentTemplateFormatSimple {uuid = format.uuid, name = format.name, icon = format.icon}}
          Nothing -> Nothing
  return $ sequence resolved

findReusableDocument :: ResolvedExport -> [Document] -> Maybe Document
findReusableDocument resolved documents =
  let candidates = filter (\d -> d.documentTemplateUuid == resolved.template.uuid && d.formatUuid == resolved.format.uuid && d.state /= ErrorDocumentState) documents
   in listToMaybe (filter ((== DoneDocumentState) . (.state)) candidates) `orElse` listToMaybe candidates
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b

toDefinitionDTO :: SettingsSubmissionDefinition -> Maybe (OAuthProvider, Bool) -> [ResolvedExport] -> SubmissionDefinitionDTO
toDefinitionDTO definition mConnection exports =
  SubmissionDefinitionDTO
    { sId = definition.sId
    , name = definition.name
    , description = definition.description
    , targetType = targetTypeName definition.target
    , exports =
        [ SubmissionDefinitionExportDTO
          { documentTemplateUuid = resolved.template.uuid
          , documentTemplateName = resolved.template.name
          , documentTemplateId = resolved.template.id
          , documentTemplateVersion = resolved.template.version
          , formatUuid = resolved.format.uuid
          , formatName = resolved.format.name
          }
        | resolved <- exports
        ]
    , oauthProvider = fmap (providerKey . fst) mConnection
    , requiresConnection = maybe False (not . snd) mConnection
    , connectOnSubmit = definition.connectOnSubmit
    }

sendSubmission :: WizardRequestContextC s m => Submission -> [Document] -> m ()
sendSubmission submission documents = do
  project <- findProjectByUuid submission.projectUuid
  settings <- getEffectiveSettingsSubmission (Just project.workspaceUuid)
  case findDefinition submission.definitionId settings of
    Nothing -> failSubmission submission documents "The submission definition does not exist anymore"
    Just definition -> do
      result <-
        case definition.target of
          HttpSubmissionTarget {request = request} -> submitToHttpTarget submission project definition request documents
          ZenodoSubmissionTarget {repository = repository, resourceType = resourceType, licenseId = licenseId, accessLevel = accessLevel, community = community} ->
            submitToZenodo
              submission
              ZenodoTarget {repository = repository, resourceType = resourceType, licenseId = licenseId, accessLevel = accessLevel, community = community}
              project
              (zip documents (fmap (.madmpMetadata) definition.exports ++ repeat False))
      case result of
        Right SubmissionResult {..} -> do
          let updated = submission {state = DoneSubmissionState, location = location, returnedData = returnedData, externalId = externalId, targetBaseUrl = targetBaseUrl} :: Submission
          saved <- updateSubmissionByUuid updated
          forM_ documents (\document -> al_submitDocument document definition saved)
          emitEvent (ProjectSubmittedEvent project saved)
        Left error -> do
          let updated = submission {state = ErrorSubmissionState, returnedData = Just error} :: Submission
          saved <- updateSubmissionByUuid updated
          forM_ documents (\document -> al_submitDocument document definition saved)
          emitEvent (ProjectSubmissionFailedEvent project saved (Just error))

failSubmission :: WizardRequestContextC s m => Submission -> [Document] -> String -> m ()
failSubmission submission _ error = do
  project <- findProjectByUuid submission.projectUuid
  saved <- updateSubmissionByUuid (submission {state = ErrorSubmissionState, returnedData = Just error} :: Submission)
  emitEvent (ProjectSubmissionFailedEvent project saved (Just error))

-- Sends the documents one by one (one request per document); the location is the one returned for the last document
-- With an OAuth authorization, the access token of the user is ${accessToken} in the request (next to the user props)
submitToHttpTarget :: WizardRequestContextC s m => Submission -> Project -> SettingsSubmissionDefinition -> SettingsSubmissionServiceRequest -> [Document] -> m (Either String SubmissionResult)
submitToHttpTarget submission project definition request documents = do
  userProps <- getUserProps definition
  eAccessToken <-
    case (definitionOAuthProvider definition, submission.createdBy) of
      (Just provider, Just userUuid) -> fmap Just <$> getValidAccessToken userUuid (Just project.workspaceUuid) provider
      (Just _, Nothing) -> return (Left "The user who created the submission does not exist anymore.")
      (Nothing, _) -> return (Right Nothing)
  case eAccessToken of
    Left error -> return (Left error)
    Right mAccessToken -> sendDocuments (maybe userProps (\token -> M.insert "accessToken" token userProps) mAccessToken)
  where
   sendDocuments userProps = do
    let go mLocation [] = return . Right $ SubmissionResult {location = mLocation, returnedData = Nothing, externalId = Nothing, targetBaseUrl = Nothing}
        go mLocation (document : rest) = do
          content <- retrieveDocumentContent document.uuid
          response <- uploadDocument request userProps content
          case response of
            Right mNewLocation -> go (maybe mLocation Just mNewLocation) rest
            Left error -> return . Left $ "Sending the document '" ++ document.name ++ "' failed: " ++ error
    go Nothing documents

getUserProps :: WizardRequestContextC s m => SettingsSubmissionDefinition -> m (M.Map String String)
getUserProps definition = do
  mUser <- asks (.currentUser')
  case mUser of
    Just user -> do
      submissionProps <- getUserProfileSubmissionProps user.uuid
      return . maybe M.empty (.values) $ L.find (\p -> p.sId == definition.sId) submissionProps
    Nothing -> return M.empty
