module Shared.Integration.Http.Zenodo.ZenodoOAuth (
  ZenodoTokenResponse (..),
  buildAuthorizeUrl,
  exchangeAuthorizationCode,
  refreshAccessToken,
) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Network.HTTP.Types.URI (renderSimpleQuery)

import Shared.Integration.Http.Zenodo.ZenodoHttp
import Shared.Model.Context.WizardRequestContext

data ZenodoTokenResponse = ZenodoTokenResponse
  { accessToken :: String
  , refreshToken :: Maybe String
  , expiresIn :: Maybe Int
  , scope :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON ZenodoTokenResponse where
  parseJSON = withObject "ZenodoTokenResponse" $ \o -> do
    accessToken <- o .: "access_token"
    refreshToken <- o .:? "refresh_token"
    expiresIn <- o .:? "expires_in"
    scope <- o .:? "scope"
    return ZenodoTokenResponse {..}

buildAuthorizeUrl :: String -> String -> String -> String -> String -> String -> String
buildAuthorizeUrl baseUrl clientId redirectUri scopes state codeChallenge =
  baseUrl
    ++ "/oauth/authorize"
    ++ BS.unpack
      ( renderSimpleQuery
          True
          [ ("response_type", "code")
          , ("client_id", BS.pack clientId)
          , ("redirect_uri", BS.pack redirectUri)
          , ("scope", BS.pack scopes)
          , ("state", BS.pack state)
          , ("code_challenge", BS.pack codeChallenge)
          , ("code_challenge_method", "S256")
          ]
      )

exchangeAuthorizationCode :: WizardRequestContextC s m => String -> String -> String -> String -> String -> String -> m (Either String ZenodoTokenResponse)
exchangeAuthorizationCode baseUrl clientId clientSecret redirectUri code codeVerifier =
  requestToken
    "Zenodo OAuth token exchange"
    baseUrl
    [ ("grant_type", "authorization_code")
    , ("client_id", clientId)
    , ("client_secret", clientSecret)
    , ("code", code)
    , ("redirect_uri", redirectUri)
    , ("code_verifier", codeVerifier)
    ]

refreshAccessToken :: WizardRequestContextC s m => String -> String -> String -> String -> m (Either String ZenodoTokenResponse)
refreshAccessToken baseUrl clientId clientSecret token =
  requestToken
    "Zenodo OAuth token refresh"
    baseUrl
    [ ("grant_type", "refresh_token")
    , ("client_id", clientId)
    , ("client_secret", clientSecret)
    , ("refresh_token", token)
    ]

requestToken :: WizardRequestContextC s m => String -> String -> [(String, String)] -> m (Either String ZenodoTokenResponse)
requestToken operation baseUrl form = do
  let headers = M.fromList [("Content-Type", "application/x-www-form-urlencoded"), ("Accept", "application/json")]
  let reqBody = renderSimpleQuery False [(BS.pack key, BS.pack value) | (key, value) <- form]
  eResponse <- zenodoRequest operation "POST" (baseUrl ++ "/oauth/token") headers reqBody
  return (eResponse >>= expectJson operation [200])
