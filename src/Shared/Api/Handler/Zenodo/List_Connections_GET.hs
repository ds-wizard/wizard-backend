module Shared.Api.Handler.Zenodo.List_Connections_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Zenodo.ZenodoConnectionDTO
import Shared.Api.Resource.Zenodo.ZenodoConnectionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Zenodo.ZenodoAuthService

type List_Connections_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "zenodo-connections"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [ZenodoConnectionDTO])

list_connections_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [ZenodoConnectionDTO])
list_connections_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getZenodoConnections
