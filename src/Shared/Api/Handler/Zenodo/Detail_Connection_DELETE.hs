module Shared.Api.Handler.Zenodo.Detail_Connection_DELETE where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Model.Zenodo.ZenodoRepository
import Shared.Service.Zenodo.ZenodoAuthService

type Detail_Connection_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "zenodo-connections"
    :> Capture "repository" String
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_connection_DELETE
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> String
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_connection_DELETE mTokenHeader mServerUrl repositoryName =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        case readZenodoRepository repositoryName of
          Just repository -> disconnectZenodo repository
          Nothing -> throwError . UserError $ _ERROR_SERVICE_ZENODO__REPOSITORY_NOT_AVAILABLE repositoryName
        return NoContent
