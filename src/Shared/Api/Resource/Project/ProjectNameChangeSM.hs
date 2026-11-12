module Shared.Api.Resource.Project.ProjectNameChangeSM where

import Data.Swagger

import Shared.Api.Resource.Project.ProjectNameChangeDTO
import Shared.Api.Resource.Project.ProjectNameChangeJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Util.Swagger

instance ToSchema ProjectNameChangeDTO where
  declareNamedSchema = toSwagger project1NameChange
