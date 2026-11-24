module Shared.Api.Handler.OAuth.Detail_Connection_DELETE where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.OAuth.OAuthService

type Detail_Connection_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "oauth-connections"
    :> Capture "provider" String
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_connection_DELETE
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> String
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_connection_DELETE mTokenHeader mServerUrl provider =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        disconnectOAuth provider
        return NoContent
