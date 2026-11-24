module Shared.Database.Mapping.OAuth.UserOAuthCredential where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.OAuth.UserOAuthCredential

instance ToRow UserOAuthCredential where
  toRow UserOAuthCredential {..} =
    [ toField userUuid
    , toField provider
    , toField accessToken
    , toField refreshToken
    , toField scope
    , toField expiresAt
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow UserOAuthCredential where
  fromRow = do
    userUuid <- field
    provider <- field
    accessToken <- field
    refreshToken <- field
    scope <- field
    expiresAt <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return UserOAuthCredential {..}
