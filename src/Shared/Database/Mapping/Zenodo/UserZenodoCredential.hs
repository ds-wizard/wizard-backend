module Shared.Database.Mapping.Zenodo.UserZenodoCredential where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.Mapping.Zenodo.ZenodoRepository ()
import Shared.Model.Zenodo.UserZenodoCredential

instance ToRow UserZenodoCredential where
  toRow UserZenodoCredential {..} =
    [ toField userUuid
    , toField repository
    , toField accessToken
    , toField refreshToken
    , toField scope
    , toField expiresAt
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow UserZenodoCredential where
  fromRow = do
    userUuid <- field
    repository <- field
    accessToken <- field
    refreshToken <- field
    scope <- field
    expiresAt <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return UserZenodoCredential {..}
