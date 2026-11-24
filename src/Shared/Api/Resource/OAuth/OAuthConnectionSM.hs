module Shared.Api.Resource.OAuth.OAuthConnectionSM where

import Data.Swagger

import Shared.Api.Resource.OAuth.OAuthConnectionDTO
import Shared.Api.Resource.OAuth.OAuthConnectionJM ()
import Shared.Util.Aeson

instance ToSchema OAuthConnectionDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema OAuthAuthorizeDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema OAuthAuthorizationDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema OAuthCallbackDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema OAuthCallbackResultDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)
