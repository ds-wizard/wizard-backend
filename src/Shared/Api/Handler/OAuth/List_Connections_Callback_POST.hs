module Shared.Api.Handler.OAuth.List_Connections_Callback_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.OAuth.OAuthConnectionDTO
import Shared.Api.Resource.OAuth.OAuthConnectionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.OAuth.OAuthCallbackService

type List_Connections_Callback_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] OAuthCallbackDTO
    :> "users"
    :> "current"
    :> "oauth-connections"
    :> "callback"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OAuthCallbackResultDTO)

list_connections_callback_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> OAuthCallbackDTO
  -> sm (Headers '[Header "x-trace-uuid" String] OAuthCallbackResultDTO)
list_connections_callback_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< completeOAuthAuthorization reqDto
