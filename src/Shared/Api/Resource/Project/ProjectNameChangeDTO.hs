module Shared.Api.Resource.Project.ProjectNameChangeDTO where

import GHC.Generics

newtype ProjectNameChangeDTO = ProjectNameChangeDTO
  { name :: String
  }
  deriving (Show, Eq, Generic)
