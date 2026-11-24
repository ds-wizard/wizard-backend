module Shared.Api.Resource.Zenodo.ZenodoConnectionJM where

import Data.Aeson

import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Api.Resource.Zenodo.ZenodoConnectionDTO
import Shared.Util.Aeson

instance FromJSON ZenodoConnectionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ZenodoConnectionDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ZenodoAuthorizeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ZenodoAuthorizeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ZenodoAuthorizationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ZenodoAuthorizationDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ZenodoCallbackDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ZenodoCallbackDTO where
  toJSON = genericToJSON jsonOptions
