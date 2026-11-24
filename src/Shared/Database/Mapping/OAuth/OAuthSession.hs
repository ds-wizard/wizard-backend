module Shared.Database.Mapping.OAuth.OAuthSession where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.OAuth.OAuthSession

instance ToRow OAuthSession where
  toRow OAuthSession {..} =
    [ toField state
    , toField userUuid
    , toField provider
    , toField workspaceUuid
    , toField codeVerifier
    , toField returnUrl
    , toField projectUuid
    , toField definitionId
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow OAuthSession where
  fromRow = do
    state <- field
    userUuid <- field
    provider <- field
    workspaceUuid <- field
    codeVerifier <- field
    returnUrl <- field
    projectUuid <- field
    definitionId <- field
    tenantUuid <- field
    createdAt <- field
    return OAuthSession {..}
