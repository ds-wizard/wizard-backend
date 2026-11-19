module Shared.Model.Context.RequestContextHelpers where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, asks, liftIO, local)
import Database.PostgreSQL.Simple.Transaction (newSavepoint, releaseSavepoint, rollbackToSavepoint)

import qualified Data.UUID as U
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Common (runDB)
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.Migration.Development.User.Data.SystemUser
import Shared.Localization.Messages.User.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import qualified Shared.Model.User.Role as Role
import Shared.Model.User.RoleSimple
import qualified Shared.Service.User.WizardUserMapper as UM
import Shared.Util.Error (tryError)
import Shared.Util.Exception (tryNonAsync)

getCurrentUser :: WizardRequestContextC s m => m UserDTO
getCurrentUser = do
  mCurrentUser <- asks (.currentUser')
  case mCurrentUser of
    Just user -> return user
    Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

getCurrentUserUuid :: WizardRequestContextC s m => m (Maybe U.UUID)
getCurrentUserUuid = do
  mCurrentUser <- asks (.currentUser')
  return . fmap (.uuid) $ mCurrentUser

isCurrentUserAdmin :: WizardRequestContextC s m => m Bool
isCurrentUserAdmin = do
  mUser <- asks (.currentUser')
  case mUser of
    Just user -> do
      role <- findRoleByUuid user.role.uuid
      return role.isAdmin
    Nothing -> return False

asSystemUser :: WizardRequestContextC s m => m a -> m a
asSystemUser = local (setCurrentUser (Just . UM.toDTO $ userSystem))

withSavepoint :: forall s m a. WizardRequestContextC s m => (a -> Bool) -> m a -> m (Either String a)
withSavepoint keep action = do
  savepoint <- runDB newSavepoint
  context <- ask
  result <- liftIO . tryNonAsync $ runRequestContextWithRequestContext @s @m (tryError action) context
  let outcome =
        case result of
          Right (Right (Right value)) -> Right value
          Right (Right (Left appError)) -> Left (show appError)
          Right (Left message) -> Left message
          Left exception -> Left (show exception)
  if either (const False) keep outcome
    then runDB (`releaseSavepoint` savepoint)
    else runDB (`rollbackToSavepoint` savepoint)
  return outcome
