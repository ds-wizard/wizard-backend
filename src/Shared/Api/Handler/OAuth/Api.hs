module Shared.Api.Handler.OAuth.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.OAuth.Detail_Connection_DELETE
import Shared.Api.Handler.OAuth.List_Connections_Authorize_POST
import Shared.Api.Handler.OAuth.List_Connections_Callback_POST
import Shared.Api.Handler.OAuth.List_Connections_GET
import Shared.Api.Handler.WizardCommon

type OAuthConnectionAPI =
  Tags "OAuth Connection"
    :> ( List_Connections_GET
           :<|> List_Connections_Authorize_POST
           :<|> List_Connections_Callback_POST
           :<|> Detail_Connection_DELETE
       )

oauthConnectionApi :: Proxy OAuthConnectionAPI
oauthConnectionApi = Proxy

oauthConnectionServer :: WizardHandlerC s sm r rm => ServerT OAuthConnectionAPI sm
oauthConnectionServer = list_connections_GET :<|> list_connections_authorize_POST :<|> list_connections_callback_POST :<|> detail_connection_DELETE
