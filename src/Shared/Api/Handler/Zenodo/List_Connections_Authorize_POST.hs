module Shared.Api.Handler.Zenodo.List_Connections_Authorize_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Zenodo.ZenodoConnectionDTO
import Shared.Api.Resource.Zenodo.ZenodoConnectionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Zenodo.ZenodoAuthService

type List_Connections_Authorize_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ZenodoAuthorizeDTO
    :> "users"
    :> "current"
    :> "zenodo-connections"
    :> "authorize"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ZenodoAuthorizationDTO)

list_connections_authorize_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ZenodoAuthorizeDTO
  -> sm (Headers '[Header "x-trace-uuid" String] ZenodoAuthorizationDTO)
list_connections_authorize_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< authorizeZenodo reqDto
