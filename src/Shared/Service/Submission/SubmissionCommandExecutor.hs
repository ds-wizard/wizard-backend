module Shared.Service.Submission.SubmissionCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Constant.PersistentCommand (submissionComponent, submissionSubmitFunction)
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.Trigger.TriggerEntityUuidCommand
import Shared.Service.Submission.SubmissionService
import Shared.Util.String (f')

cComponent = submissionComponent

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == submissionSubmitFunction = cSubmit command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

-- The command is created when a submission is created and whenever one of its documents is finished; it sends the
-- submission once all its documents are done (or fails it), otherwise it does nothing
cSubmit :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cSubmit persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String TriggerEntityUuidCommand
  case eCommand of
    Right command -> do
      finishSubmission command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
