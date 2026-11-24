module Shared.Database.Mapping.Zenodo.ZenodoOAuthSession where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.Mapping.Zenodo.ZenodoRepository ()
import Shared.Model.Zenodo.ZenodoOAuthSession

instance ToRow ZenodoOAuthSession where
  toRow ZenodoOAuthSession {..} =
    [ toField state
    , toField userUuid
    , toField repository
    , toField codeVerifier
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow ZenodoOAuthSession where
  fromRow = do
    state <- field
    userUuid <- field
    repository <- field
    codeVerifier <- field
    tenantUuid <- field
    createdAt <- field
    return ZenodoOAuthSession {..}
