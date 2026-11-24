-- Completes an OAuth authorization and, when it was started by a submission attempted without the connection, creates
-- that submission (a failed submission does not undo the connection; the user is told why)
module Shared.Service.OAuth.OAuthCallbackService (
  completeOAuthAuthorization,
) where


import Shared.Api.Resource.OAuth.OAuthConnectionDTO
import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.OAuth.OAuthSession
import Shared.Model.Submission.SubmissionList
import Shared.Service.OAuth.OAuthService
import Shared.Service.Submission.SubmissionService
import Shared.Util.Error (tryError)

completeOAuthAuthorization :: WizardRequestContextC s m => OAuthCallbackDTO -> m OAuthCallbackResultDTO
completeOAuthAuthorization reqDto =
  runInTransaction $ do
    (connection, session) <- exchangeOAuthAuthorization reqDto
    eSubmission <-
      case (session.projectUuid, session.definitionId) of
        (Just projectUuid, Just definitionId) -> do
          result <- withSavepoint (either (const False) (const True)) (tryError (createSubmission projectUuid (SubmissionCreateDTO definitionId)))
          return . Just $
            case result of
              Right (Right submission) -> Right submission
              Right (Left appError) -> Left (appErrorMessage appError)
              Left message -> Left message
        _ -> return Nothing
    return
      OAuthCallbackResultDTO
        { connection = connection
        , returnUrl = session.returnUrl
        , submissionUuid = eSubmission >>= either (const Nothing) (Just . (.uuid))
        , submissionError = eSubmission >>= either Just (const Nothing)
        }
