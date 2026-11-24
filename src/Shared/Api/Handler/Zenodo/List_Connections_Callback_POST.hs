module Shared.Api.Handler.Zenodo.List_Connections_Callback_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Zenodo.ZenodoConnectionDTO
import Shared.Api.Resource.Zenodo.ZenodoConnectionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Zenodo.ZenodoAuthService

type List_Connections_Callback_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ZenodoCallbackDTO
    :> "users"
    :> "current"
    :> "zenodo-connections"
    :> "callback"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ZenodoConnectionDTO)

list_connections_callback_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ZenodoCallbackDTO
  -> sm (Headers '[Header "x-trace-uuid" String] ZenodoConnectionDTO)
list_connections_callback_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< completeZenodoAuthorization reqDto
