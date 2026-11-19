module Shared.Service.User.UserNotification where

import Control.Monad (when)

import Shared.Api.Resource.User.UserDTO
import Shared.Constant.Component
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.RoleSimple
import Shared.Model.User.User
import Shared.Service.Mail.Mailer
import Shared.Util.Logger

notifyTenantRoleChanged :: WizardRequestContextC s m => RoleSimple -> User -> m ()
notifyTenantRoleChanged oldRole user =
  swallowMailError . whenUserNotified user $
    when (oldRole.uuid /= user.role.uuid) (sendTenantRoleChangedMail user oldRole user.role)

whenUserNotified :: WizardRequestContextC s m => User -> m () -> m ()
whenUserNotified user action = do
  currentUser <- getCurrentUser
  when (user.active && user.uuid /= currentUser.uuid) action

swallowMailError :: WizardRequestContextC s m => m () -> m ()
swallowMailError action = do
  result <- withSavepoint (const True) action
  either (\failure -> logWarnI _CMP_SERVICE ("Notification mail not sent: " ++ failure)) return result
