module Shared.Service.Automation.AutomationEventService where

import Control.Monad (unless)
import Data.Foldable (traverse_)

import Shared.Model.Automation.AutomationEvent
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Project
import Shared.Model.User.User

emitProjectUpdated :: WizardRequestContextC s m => Project -> Project -> m ()
emitProjectUpdated project updatedProject = do
  let changedFields = projectChangedFields project updatedProject
  unless (null changedFields) (emitEvent (ProjectUpdatedEvent updatedProject changedFields))

emitProjectPermissionsChanged :: WizardRequestContextC s m => Project -> Project -> m ()
emitProjectPermissionsChanged project updatedProject =
  traverse_ (\(perm, change) -> emitEvent (ProjectPermissionChangedEvent updatedProject perm change)) (projectPermissionChanges project updatedProject)

emitUserUpdated :: WizardRequestContextC s m => User -> User -> m ()
emitUserUpdated user updatedUser = do
  let changedFields = userChangedFields user updatedUser
  unless (null changedFields) (emitEvent (UserUpdatedEvent updatedUser changedFields))
