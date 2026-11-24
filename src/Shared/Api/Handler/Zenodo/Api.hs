module Shared.Api.Handler.Zenodo.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import Shared.Api.Handler.Zenodo.Detail_Connection_DELETE
import Shared.Api.Handler.Zenodo.List_Connections_Authorize_POST
import Shared.Api.Handler.Zenodo.List_Connections_Callback_POST
import Shared.Api.Handler.Zenodo.List_Connections_GET

type ZenodoAPI =
  Tags "Zenodo"
    :> ( List_Connections_GET
           :<|> List_Connections_Authorize_POST
           :<|> List_Connections_Callback_POST
           :<|> Detail_Connection_DELETE
       )

zenodoApi :: Proxy ZenodoAPI
zenodoApi = Proxy

zenodoServer :: WizardHandlerC s sm r rm => ServerT ZenodoAPI sm
zenodoServer = list_connections_GET :<|> list_connections_authorize_POST :<|> list_connections_callback_POST :<|> detail_connection_DELETE
