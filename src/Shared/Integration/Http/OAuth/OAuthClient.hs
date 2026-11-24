-- The OAuth 2.0 authorization code flow (with PKCE) of a provider: Zenodo or the OAuth of an HTTP submission target
module Shared.Integration.Http.OAuth.OAuthClient (
  OAuthTokenResponse (..),
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

data OAuthTokenResponse = OAuthTokenResponse
  { accessToken :: String
  , refreshToken :: Maybe String
  , expiresIn :: Maybe Int
  , scope :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON OAuthTokenResponse where
  parseJSON = withObject "OAuthTokenResponse" $ \o -> do
    accessToken <- o .: "access_token"
    refreshToken <- o .:? "refresh_token"
    expiresIn <- o .:? "expires_in"
    scope <- o .:? "scope"
    return OAuthTokenResponse {..}

-- The authorize URL may already have a query (some providers need fixed parameters there)
buildAuthorizeUrl :: String -> String -> String -> String -> String -> String -> String
buildAuthorizeUrl authorizeUrl clientId redirectUri scopes state codeChallenge =
  let query =
        renderSimpleQuery
          False
          [ ("response_type", "code")
          , ("client_id", BS.pack clientId)
          , ("redirect_uri", BS.pack redirectUri)
          , ("scope", BS.pack scopes)
          , ("state", BS.pack state)
          , ("code_challenge", BS.pack codeChallenge)
          , ("code_challenge_method", "S256")
          ]
      separator = if '?' `elem` authorizeUrl then "&" else "?"
   in authorizeUrl ++ separator ++ BS.unpack query

exchangeAuthorizationCode :: WizardRequestContextC s m => String -> String -> String -> String -> String -> String -> m (Either String OAuthTokenResponse)
exchangeAuthorizationCode tokenUrl clientId clientSecret redirectUri code codeVerifier =
  requestToken
    "OAuth token exchange"
    tokenUrl
    [ ("grant_type", "authorization_code")
    , ("client_id", clientId)
    , ("client_secret", clientSecret)
    , ("code", code)
    , ("redirect_uri", redirectUri)
    , ("code_verifier", codeVerifier)
    ]

refreshAccessToken :: WizardRequestContextC s m => String -> String -> String -> String -> m (Either String OAuthTokenResponse)
refreshAccessToken tokenUrl clientId clientSecret token =
  requestToken
    "OAuth token refresh"
    tokenUrl
    [ ("grant_type", "refresh_token")
    , ("client_id", clientId)
    , ("client_secret", clientSecret)
    , ("refresh_token", token)
    ]

requestToken :: WizardRequestContextC s m => String -> String -> [(String, String)] -> m (Either String OAuthTokenResponse)
requestToken operation tokenUrl form = do
  let headers = M.fromList [("Content-Type", "application/x-www-form-urlencoded"), ("Accept", "application/json")]
  let reqBody = renderSimpleQuery False [(BS.pack key, BS.pack value) | (key, value) <- form]
  eResponse <- zenodoRequest operation "POST" tokenUrl headers reqBody
  return (eResponse >>= expectJson operation [200])
