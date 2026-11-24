module Shared.Api.Handler.Submission.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Submission.SubmissionList
import Shared.Service.Submission.SubmissionService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "submissions"
    :> Capture "submissionUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] SubmissionList)

detail_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] SubmissionList)
detail_GET mTokenHeader mServerUrl projectUuid submissionUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getSubmissionForProject projectUuid submissionUuid
