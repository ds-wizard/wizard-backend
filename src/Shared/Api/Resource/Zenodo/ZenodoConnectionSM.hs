module Shared.Api.Resource.Zenodo.ZenodoConnectionSM where

import Data.Swagger

import Shared.Api.Resource.Settings.SettingsSM ()
import Shared.Api.Resource.Zenodo.ZenodoConnectionDTO
import Shared.Api.Resource.Zenodo.ZenodoConnectionJM ()
import Shared.Util.Aeson

instance ToSchema ZenodoConnectionDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema ZenodoAuthorizeDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema ZenodoAuthorizationDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema ZenodoCallbackDTO where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)
