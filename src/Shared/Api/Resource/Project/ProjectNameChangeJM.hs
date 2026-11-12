module Shared.Api.Resource.Project.ProjectNameChangeJM where

import Data.Aeson

import Shared.Api.Resource.Project.ProjectNameChangeDTO
import Shared.Util.Aeson

instance FromJSON ProjectNameChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectNameChangeDTO where
  toJSON = genericToJSON jsonOptions
