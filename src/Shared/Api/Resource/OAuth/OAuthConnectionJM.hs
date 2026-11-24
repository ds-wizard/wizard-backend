module Shared.Api.Resource.OAuth.OAuthConnectionJM where

import Data.Aeson

import Shared.Api.Resource.OAuth.OAuthConnectionDTO
import Shared.Util.Aeson

instance FromJSON OAuthConnectionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OAuthConnectionDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OAuthAuthorizeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OAuthAuthorizeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OAuthAuthorizationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OAuthAuthorizationDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OAuthCallbackDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OAuthCallbackDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OAuthCallbackResultDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OAuthCallbackResultDTO where
  toJSON = genericToJSON jsonOptions
