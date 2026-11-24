module Shared.Api.Handler.Submission.List_Available_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Submission.SubmissionDefinitionDTO
import Shared.Api.Resource.Submission.SubmissionDefinitionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Submission.SubmissionService

type List_Available_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "available-submissions"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [SubmissionDefinitionDTO])

list_available_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] [SubmissionDefinitionDTO])
list_available_GET mTokenHeader mServerUrl projectUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getAvailableSubmissions projectUuid
