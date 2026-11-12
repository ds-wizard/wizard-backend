module Shared.Service.Project.Event.ProjectEventStorageService where

import Control.Monad (mfilter, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectCommentDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Localization.Messages.Public
import Shared.Model.Common.Lens
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Event.ProjectEventLenses ()
import Shared.Model.User.User
import Shared.Model.User.UserSuggestion
import Shared.Service.Project.Comment.ProjectCommentMapper
import Shared.Service.Project.Event.ProjectEventMapper

storeEvent :: WizardRequestContextC s m => U.UUID -> Maybe UserSuggestion -> UTCTime -> ProjectEventChangeDTO -> m ()
storeEvent projectUuid mCreatedBy now reqDto = do
  traverse_ (checkThreadOfProject projectUuid) (existingThreadUuid reqDto)
  storeEventChange projectUuid (fmap (.uuid) mCreatedBy) now reqDto

storeEventOnce :: WizardRequestContextC s m => U.UUID -> Maybe UserSuggestion -> UTCTime -> ProjectEventChangeDTO -> m Bool
storeEventOnce projectUuid mCreatedBy now reqDto
  | isQuestionnaireEvent reqDto = do
      tenantUuid <- asks (.tenantUuid')
      insertProjectEventIfAbsent (fromEventChangeDTO reqDto projectUuid tenantUuid (fmap (.uuid) mCreatedBy) now)
  | otherwise = storeEvent projectUuid mCreatedBy now reqDto >> return True

findStoredEvent :: WizardRequestContextC s m => U.UUID -> ProjectEventChangeDTO -> m (Maybe ProjectEventDTO)
findStoredEvent projectUuid (AddCommentEventChangeDTO' event) = findStoredComment projectUuid event
findStoredEvent projectUuid reqDto
  | isQuestionnaireEvent reqDto = do
      mEvent <- findProjectEventByProjectUuidAndUuid' projectUuid (eventChangeUuid reqDto)
      traverse (\event -> toEventDTO event <$> findCreatedBy (getCreatedBy event)) mEvent
  | otherwise = return Nothing

findStoredComment :: WizardRequestContextC s m => U.UUID -> AddCommentEventChangeDTO -> m (Maybe ProjectEventDTO)
findStoredComment projectUuid event = do
  mThread <- findProjectCommentThreadById event.threadUuid
  let mStored = do
        thread <- mfilter ((== projectUuid) . (.projectUuid)) mThread
        comment <- L.find ((== event.commentUuid) . (.uuid)) thread.comments
        return (thread, comment)
  traverse (\(thread, comment) -> AddCommentEventDTO' . toAddCommentEventDTO event thread comment <$> findCreatedBy comment.createdBy) mStored

findCreatedBy :: WizardRequestContextC s m => Maybe U.UUID -> m (Maybe User)
findCreatedBy = maybe (return Nothing) findUserByUuid'

storeEventChange :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> UTCTime -> ProjectEventChangeDTO -> m ()
storeEventChange _ _ _ (ResolveCommentThreadEventChangeDTO' event) = void $ updateProjectCommentThreadResolvedById event.threadUuid True
storeEventChange _ _ _ (ReopenCommentThreadEventChangeDTO' event) = void $ updateProjectCommentThreadResolvedById event.threadUuid False
storeEventChange _ mCreatedByUuid _ (AssignCommentThreadEventChangeDTO' event) =
  void $ updateProjectCommentThreadAssignee event.threadUuid (fmap (.uuid) event.assignedTo) mCreatedByUuid
storeEventChange projectUuid _ _ (DeleteCommentThreadEventChangeDTO' event) = deleteCommentThread projectUuid event.threadUuid
storeEventChange projectUuid mCreatedByUuid now (AddCommentEventChangeDTO' event) = addComment projectUuid mCreatedByUuid now event
storeEventChange _ _ _ (EditCommentEventChangeDTO' event) = void $ updateProjectCommentTextById event.threadUuid event.commentUuid event.text
storeEventChange projectUuid _ _ (DeleteCommentEventChangeDTO' event) = do
  deleteProjectCommentById event.threadUuid event.commentUuid
  void $ deleteProjectCacheByProjectUuid projectUuid
storeEventChange projectUuid mCreatedByUuid now reqDto = do
  tenantUuid <- asks (.tenantUuid')
  insertProjectEventWithTimestampUpdate projectUuid (fromEventChangeDTO reqDto projectUuid tenantUuid mCreatedByUuid now)

deleteCommentThread :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteCommentThread projectUuid threadUuid = do
  deleteProjectCommentsByThreadUuid threadUuid
  deleteProjectCommentThreadById threadUuid
  void $ deleteProjectCacheByProjectUuid projectUuid

addComment :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> UTCTime -> AddCommentEventChangeDTO -> m ()
addComment projectUuid mCreatedByUuid now event = do
  tenantUuid <- asks (.tenantUuid')
  let comment = toComment event tenantUuid mCreatedByUuid now
  if event.newThread
    then void $ insertProjectThreadAndComment (toCommentThread event projectUuid tenantUuid mCreatedByUuid now) comment
    else void $ insertProjectComment comment

checkThreadOfProject :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
checkThreadOfProject projectUuid threadUuid = do
  mThread <- findProjectCommentThreadById threadUuid
  when (any ((/= projectUuid) . (.projectUuid)) mThread) $
    throwError . NotExistsError $
      _ERROR_DATABASE__ENTITY_NOT_FOUND "project_comment_thread" [("project_uuid", U.toString projectUuid), ("uuid", U.toString threadUuid)]

existingThreadUuid :: ProjectEventChangeDTO -> Maybe U.UUID
existingThreadUuid reqDto =
  case reqDto of
    ResolveCommentThreadEventChangeDTO' event -> Just event.threadUuid
    ReopenCommentThreadEventChangeDTO' event -> Just event.threadUuid
    AssignCommentThreadEventChangeDTO' event -> Just event.threadUuid
    DeleteCommentThreadEventChangeDTO' event -> Just event.threadUuid
    AddCommentEventChangeDTO' event | not event.newThread -> Just event.threadUuid
    EditCommentEventChangeDTO' event -> Just event.threadUuid
    DeleteCommentEventChangeDTO' event -> Just event.threadUuid
    _ -> Nothing

isQuestionnaireEvent :: ProjectEventChangeDTO -> Bool
isQuestionnaireEvent reqDto =
  case reqDto of
    SetReplyEventChangeDTO' _ -> True
    ClearReplyEventChangeDTO' _ -> True
    SetPhaseEventChangeDTO' _ -> True
    SetLabelsEventChangeDTO' _ -> True
    _ -> False

eventChangeUuid :: ProjectEventChangeDTO -> U.UUID
eventChangeUuid reqDto =
  case reqDto of
    SetReplyEventChangeDTO' event -> event.uuid
    ClearReplyEventChangeDTO' event -> event.uuid
    SetPhaseEventChangeDTO' event -> event.uuid
    SetLabelsEventChangeDTO' event -> event.uuid
    ResolveCommentThreadEventChangeDTO' event -> event.uuid
    ReopenCommentThreadEventChangeDTO' event -> event.uuid
    AssignCommentThreadEventChangeDTO' event -> event.uuid
    DeleteCommentThreadEventChangeDTO' event -> event.uuid
    AddCommentEventChangeDTO' event -> event.uuid
    EditCommentEventChangeDTO' event -> event.uuid
    DeleteCommentEventChangeDTO' event -> event.uuid
