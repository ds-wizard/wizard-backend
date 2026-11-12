module Shared.Service.Project.Collaboration.ProjectCollaborationService where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO, local)
import Data.Aeson (ToJSON, toJSON)
import Data.IORef
import Data.Maybe (isJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project
import Shared.Model.User.OnlineUserInfo
import Shared.Model.User.UserGroupMembership
import Shared.Model.User.UserSuggestion
import Shared.Model.Websocket.WebsocketBusMessage
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Service.Acl.AclService
import Shared.Service.Project.Collaboration.ProjectCollaborationAcl
import Shared.Service.Project.Collaboration.ProjectCollaborationMapper
import Shared.Service.Project.Event.ProjectEventMapper
import Shared.Service.Project.Event.ProjectEventStorageService
import Shared.Service.Project.Event.ProjectEventValidation
import Shared.Service.Project.ProjectUtil
import Shared.Service.Websocket.WebsocketBusService
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Service.Websocket.WebsocketService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.Error (tryError)
import Shared.Util.Websocket

putUserOnline :: WizardRequestContextC s m => U.UUID -> U.UUID -> WebsocketOutbound -> m ()
putUserOnline projectUuid connectionUuid outbound = do
  (mWorkspaceUser, permission, userGroupUuids) <- resolveProjectPermission projectUuid
  checkViewPermission permission
  key <- projectKey projectUuid
  mCurrentUser <- asks (.currentUser')
  let tenantRolePermissions = maybe [] (.role.permissions) mCurrentUser
  local (setCurrentUser mWorkspaceUser) (connectUser key connectionUuid outbound permission userGroupUuids tenantRolePermissions)
  logWS connectionUuid "New user added to the list"
  publishUserListChanged key

deleteUser :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteUser projectUuid connectionUuid = do
  key <- projectKey projectUuid
  present <- disconnectUser key connectionUuid
  when present (publishUserListChanged key)

updatePermsForOnlineUsers :: WizardRequestContextC s m => U.UUID -> m ()
updatePermsForOnlineUsers projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  publish ProjectPermissionsChanged {..}

updatePermsForWorkspace :: WizardRequestContextC s m => U.UUID -> m ()
updatePermsForWorkspace workspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  publish WorkspacePermissionsChanged {..}

removeUserGroupFromUsers :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> m ()
removeUserGroupFromUsers userGroupUuid userUuids = do
  tenantUuid <- asks (.tenantUuid')
  publish UserGroupRemoved {..}

setProject :: WizardRequestContextC s m => U.UUID -> ProjectDetailWsDTO -> m ()
setProject projectUuid reqDto = broadcast projectUuid AllRecipients (toSetProjectMessage reqDto)

addEvent :: WizardRequestContextC s m => Project -> WebsocketPerm -> Maybe UserSuggestion -> ProjectEventChangeDTO -> m ()
addEvent project = processSetContent project.uuid (compileProjectKnowledgeModel project.knowledgeModelPackageUuid project.selectedQuestionTagUuids)

addFile :: WizardRequestContextC s m => U.UUID -> ProjectFileSimple -> m ()
addFile projectUuid reqDto = broadcast projectUuid AllRecipients (toAddFileMessage reqDto)

logOutOnlineUsersWhenProjectDramaticallyChanged :: WizardRequestContextC s m => U.UUID -> m ()
logOutOnlineUsersWhenProjectDramaticallyChanged projectUuid = do
  (tenantUuid, subscription, entityUuid) <- projectKey projectUuid
  publish LogOutAll {..}

-- --------------------------------
setContent :: WizardRequestContextC s m => U.UUID -> IORef (Maybe KnowledgeModel) -> U.UUID -> ProjectEventChangeDTO -> m ()
setContent projectUuid knowledgeModelRef connectionUuid reqDto = do
  key <- projectKey projectUuid
  myself <- findLocalConnection' key connectionUuid
  checkChangePermission myself.websocketPerm reqDto
  mStored <- findStoredEvent projectUuid reqDto
  case mStored of
    Just stored -> echoStoredEvent myself stored
    Nothing -> addNewEvent projectUuid (findConnectionKnowledgeModel knowledgeModelRef projectUuid) myself reqDto

addNewEvent :: WizardRequestContextC s m => U.UUID -> m KnowledgeModel -> LocalWebsocketConnection -> ProjectEventChangeDTO -> m ()
addNewEvent projectUuid findKnowledgeModel myself reqDto = do
  let mCreatedBy = getMaybeCreatedBy myself.onlineUser
  validateProjectEventChange projectUuid findKnowledgeModel reqDto
  now <- liftIO getCurrentTimeInMicroseconds
  result <- tryError (storeEventOnce projectUuid mCreatedBy now reqDto)
  case result of
    Right True -> broadcastEvent projectUuid mCreatedBy now reqDto
    Right False -> echoStoredEventOr (UserError _ERROR_DATABASE__UNIQUE_CONSTRAINT_VIOLATION)
    Left error -> echoStoredEventOr error
  where
    echoStoredEventOr error = findStoredEvent projectUuid reqDto >>= maybe (throwError error) (echoStoredEvent myself)

echoStoredEvent :: WizardRequestContextC s m => LocalWebsocketConnection -> ProjectEventDTO -> m ()
echoStoredEvent myself event = do
  when (isPrivateComment event) (checkEditPermission myself.websocketPerm)
  enqueueMessage myself (toAddEventMessage event)

findConnectionKnowledgeModel :: WizardRequestContextC s m => IORef (Maybe KnowledgeModel) -> U.UUID -> m KnowledgeModel
findConnectionKnowledgeModel knowledgeModelRef projectUuid = do
  mKnowledgeModel <- liftIO (readIORef knowledgeModelRef)
  case mKnowledgeModel of
    Just km -> return km
    Nothing -> do
      project <- findProjectByUuid projectUuid
      km <- compileProjectKnowledgeModel project.knowledgeModelPackageUuid project.selectedQuestionTagUuids
      liftIO (writeIORef knowledgeModelRef (Just km))
      return km

processSetContent :: WizardRequestContextC s m => U.UUID -> m KnowledgeModel -> WebsocketPerm -> Maybe UserSuggestion -> ProjectEventChangeDTO -> m ()
processSetContent projectUuid findKnowledgeModel perm mCreatedBy reqDto = do
  checkChangePermission perm reqDto
  validateProjectEventChange projectUuid findKnowledgeModel reqDto
  now <- liftIO getCurrentTime
  storeEvent projectUuid mCreatedBy now reqDto
  broadcastEvent projectUuid mCreatedBy now reqDto

broadcastEvent :: WizardRequestContextC s m => U.UUID -> Maybe UserSuggestion -> UTCTime -> ProjectEventChangeDTO -> m ()
broadcastEvent projectUuid mCreatedBy now reqDto =
  broadcast projectUuid (maybe AllRecipients commentRecipients (commentPrivacy reqDto)) (toAddEventMessage (toEventDTO' reqDto mCreatedBy now))

checkChangePermission :: WizardRequestContextC s m => WebsocketPerm -> ProjectEventChangeDTO -> m ()
checkChangePermission perm reqDto = if isJust (commentPrivacy reqDto) then checkCommentPermission perm else checkEditPermission perm

isPrivateComment :: ProjectEventDTO -> Bool
isPrivateComment (AddCommentEventDTO' event) = event.private
isPrivateComment _ = False

commentPrivacy :: ProjectEventChangeDTO -> Maybe Bool
commentPrivacy reqDto =
  case reqDto of
    ResolveCommentThreadEventChangeDTO' event -> Just event.private
    ReopenCommentThreadEventChangeDTO' event -> Just event.private
    AssignCommentThreadEventChangeDTO' event -> Just event.private
    DeleteCommentThreadEventChangeDTO' event -> Just event.private
    AddCommentEventChangeDTO' event -> Just event.private
    EditCommentEventChangeDTO' event -> Just event.private
    DeleteCommentEventChangeDTO' event -> Just event.private
    _ -> Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
projectKey :: WizardRequestContextC s m => U.UUID -> m WebsocketEntityKey
projectKey projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  return (tenantUuid, ProjectWebsocketSubscription, projectUuid)

broadcast :: (WizardRequestContextC s m, ToJSON content) => U.UUID -> WebsocketRecipients -> content -> m ()
broadcast projectUuid recipients content = do
  (tenantUuid, subscription, entityUuid) <- projectKey projectUuid
  publish Broadcast {payload = toJSON content, ..}

commentRecipients :: Bool -> WebsocketRecipients
commentRecipients private = if private then EditorsOnly else CommentersAndEditors

resolveProjectPermission :: WizardRequestContextC s m => U.UUID -> m (Maybe UserDTO, WebsocketPerm, [U.UUID])
resolveProjectPermission projectUuid = do
  mCurrentUser <- asks (.currentUser')
  workspaceRoles <- asks (.workspaceRoles')
  project <- findProjectByUuid projectUuid
  userGroupUuids <- maybe (return []) (fmap (fmap (.userGroupUuid)) . findUserGroupMembershipsByUserUuid . (.uuid)) mCurrentUser
  multiWorkspace <- asks (.tenantMultiWorkspace')
  reachable <- isWorkspaceReachable project.workspaceUuid
  let withWorkspacePermissions currentUser = currentUser {role = currentUser.role {permissions = permissionsInWorkspace currentUser workspaceRoles multiWorkspace (Just project.workspaceUuid)}} :: UserDTO
  let mWorkspaceUser = fmap withWorkspacePermissions mCurrentUser
  let permission =
        getPermission
          project.visibility
          project.sharing
          project.permissions
          (fmap (.uuid) mWorkspaceUser)
          (maybe [] (.role.permissions) mWorkspaceUser)
          userGroupUuids
          reachable
  return (mWorkspaceUser, permission, userGroupUuids)

getMaybeCreatedBy :: OnlineUserInfo -> Maybe UserSuggestion
getMaybeCreatedBy LoggedOnlineUserInfo {..} = Just UserSuggestion {..}
getMaybeCreatedBy AnonymousOnlineUserInfo {} = Nothing
