module Shared.Api.Handler.OAuth.List_Connections_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.OAuth.OAuthConnectionDTO
import Shared.Api.Resource.OAuth.OAuthConnectionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.OAuth.OAuthService

type List_Connections_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "oauth-connections"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [OAuthConnectionDTO])

list_connections_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [OAuthConnectionDTO])
list_connections_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getOAuthConnections
