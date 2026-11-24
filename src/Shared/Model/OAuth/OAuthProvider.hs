module Shared.Model.OAuth.OAuthProvider where

import qualified Data.List as L

import Shared.Model.Settings.Settings
import Shared.Model.Zenodo.ZenodoRepository

-- Where a user connects an account with OAuth to submit: a Zenodo repository, or a submission definition whose HTTP
-- target has an OAuth authorization
data OAuthProvider
  = ZenodoOAuthProvider ZenodoRepository
  | SubmissionOAuthProvider String
  deriving (Show, Eq)

-- The key the connections are stored with (and the provider is named with in the API)
providerKey :: OAuthProvider -> String
providerKey (ZenodoOAuthProvider repository) = "zenodo:" ++ show repository
providerKey (SubmissionOAuthProvider definitionId) = "submission:" ++ definitionId

readProviderKey :: String -> Maybe OAuthProvider
readProviderKey key
  | Just repository <- L.stripPrefix "zenodo:" key = ZenodoOAuthProvider <$> readZenodoRepository repository
  | Just definitionId <- L.stripPrefix "submission:" key, not (null definitionId) = Just (SubmissionOAuthProvider definitionId)
  | otherwise = Nothing

-- The provider the target of a definition needs a connection to, if any
definitionOAuthProvider :: SettingsSubmissionDefinition -> Maybe OAuthProvider
definitionOAuthProvider definition =
  case definition.target of
    ZenodoSubmissionTarget {repository = repository} -> Just (ZenodoOAuthProvider repository)
    HttpSubmissionTarget {oauth = Just _} -> Just (SubmissionOAuthProvider definition.sId)
    HttpSubmissionTarget {oauth = Nothing} -> Nothing

-- How the authorization of a provider is done (resolved from the settings)
data OAuthProviderConfig = OAuthProviderConfig
  { name :: String
  , authorizeUrl :: String
  , tokenUrl :: String
  , clientId :: String
  , clientSecret :: String
  , scopes :: [String]
  }
  deriving (Show, Eq)
