module Shared.Api.Resource.Websocket.WebsocketActionDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Error.Error

data Success_ServerActionDTO a = Success_ServerActionDTO
  { aData :: a
  }
  deriving (Show, Eq, Generic)

data Error_ServerActionDTO = Error_ServerActionDTO
  { aData :: AppError
  , eventUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
