module Shared.Api.Handler.OAuth.List_Connections_Authorize_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.OAuth.OAuthConnectionDTO
import Shared.Api.Resource.OAuth.OAuthConnectionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.OAuth.OAuthService

type List_Connections_Authorize_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] OAuthAuthorizeDTO
    :> "users"
    :> "current"
    :> "oauth-connections"
    :> "authorize"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OAuthAuthorizationDTO)

list_connections_authorize_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> OAuthAuthorizeDTO
  -> sm (Headers '[Header "x-trace-uuid" String] OAuthAuthorizationDTO)
list_connections_authorize_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< authorizeOAuth reqDto
