-- The OAuth connections of the users to the providers the submissions need (Zenodo, HTTP targets with an OAuth
-- authorization): the authorization code flow with PKCE, the encrypted tokens and their refresh
module Shared.Service.OAuth.OAuthService (
  getOAuthConnections,
  authorizeOAuth,
  exchangeOAuthAuthorization,
  disconnectOAuth,
  getValidAccessToken,
  isConnected,
  resolveProviderConfig,
  missingScopes,
  credentialKey,
  scopeWorkspaceUuid,
  toConnectionDTO,
) where

import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.OAuth.OAuthConnectionDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.OAuth.OAuthSessionDAO
import Shared.Database.DAO.OAuth.UserOAuthCredentialDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.OAuth.OAuthClient
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.Scope
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.OAuth.OAuthProvider
import Shared.Model.OAuth.OAuthSession
import Shared.Model.OAuth.UserOAuthCredential
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.Zenodo.ZenodoRepository
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Settings.SettingsSubmissionUtil
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Service.Tenant.TenantHelper (getClientUrl)
import Shared.Util.Crypto

-- The connections of the current user to the providers configured in the scope of the request
getOAuthConnections :: WizardRequestContextC s m => m [OAuthConnectionDTO]
getOAuthConnections = do
  user <- getCurrentUser
  mWorkspaceUuid <- scopeWorkspaceUuid
  providers <- availableProviders mWorkspaceUuid
  credentials <- findUserOAuthCredentials user.uuid
  return [toConnectionDTO provider config (L.find ((== credentialKey provider config) . (.provider)) credentials) | (provider, config) <- providers]

-- Starts the authorization code flow with PKCE; the frontend redirects the user to the returned URL and the provider
-- redirects back to the submission accounts of the profile (<client url>/users/edit/current/submission-accounts),
-- which completes the flow with the code and the state. With a project and a definition, the submission is created
-- once connected (see OAuthCallbackService).
authorizeOAuth :: WizardRequestContextC s m => OAuthAuthorizeDTO -> m OAuthAuthorizationDTO
authorizeOAuth reqDto =
  runInTransaction $ do
    user <- getCurrentUser
    provider <- maybe (throwError . UserError $ _ERROR_SERVICE_OAUTH__PROVIDER_NOT_AVAILABLE reqDto.provider) return (readProviderKey reqDto.provider)
    mWorkspaceUuid <-
      case reqDto.projectUuid of
        Just projectUuid -> Just . (.workspaceUuid) <$> findProjectByUuid projectUuid
        Nothing -> scopeWorkspaceUuid
    config <- requireProviderConfig mWorkspaceUuid provider
    state <- liftIO $ generateRandomString 32
    codeVerifier <- liftIO $ generateRandomString 48
    encryptedCodeVerifier <- encrypt codeVerifier
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    _ <-
      insertOAuthSession
        OAuthSession
          { state = state
          , userUuid = user.uuid
          , provider = providerKey provider
          , workspaceUuid = mWorkspaceUuid
          , codeVerifier = encryptedCodeVerifier
          , returnUrl = reqDto.returnUrl >>= safeReturnUrl
          , projectUuid = reqDto.projectUuid
          , definitionId = reqDto.definitionId
          , tenantUuid = tenantUuid
          , createdAt = now
          }
    redirectUri <- getRedirectUri
    return OAuthAuthorizationDTO {url = buildAuthorizeUrl config.authorizeUrl config.clientId redirectUri (unwords config.scopes) state (pkceCodeChallenge codeVerifier)}

-- Exchanges the code: the state is used only once (it is consumed in its own committed transaction, so that a failed
-- exchange does not restore it), it must belong to the current user and be at most 10 minutes old
exchangeOAuthAuthorization :: WizardRequestContextC s m => OAuthCallbackDTO -> m (OAuthConnectionDTO, OAuthSession)
exchangeOAuthAuthorization reqDto = do
  user <- getCurrentUser
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  mSession <- runInPooledTransaction (takeOAuthSession reqDto.state user.uuid tenantUuid)
  session <-
    case mSession of
      Just session | diffUTCTime now session.createdAt < 600 -> return session
      _ -> throwError $ UserError _ERROR_SERVICE_OAUTH__INVALID_STATE
  provider <- maybe (throwError $ UserError _ERROR_SERVICE_OAUTH__INVALID_STATE) return (readProviderKey session.provider)
  config <- requireProviderConfig session.workspaceUuid provider
  codeVerifier <- decrypt session.codeVerifier
  redirectUri <- getRedirectUri
  eToken <- exchangeAuthorizationCode config.tokenUrl config.clientId config.clientSecret redirectUri reqDto.code codeVerifier
  token <- either (throwError . UserError . _ERROR_SERVICE_OAUTH__CONNECTION_FAILED) return eToken
  credential <- storeToken user.uuid provider config Nothing token
  return (toConnectionDTO provider config (Just credential), session)

disconnectOAuth :: WizardRequestContextC s m => String -> m ()
disconnectOAuth key = do
  user <- getCurrentUser
  deleteUserOAuthCredentialsOfProvider user.uuid key

-- Whether the user has a connection to the provider with all the permissions it asks for
isConnected :: WizardRequestContextC s m => U.UUID -> OAuthProvider -> OAuthProviderConfig -> m Bool
isConnected userUuid provider config = do
  mCredential <- findUserOAuthCredential userUuid (credentialKey provider config)
  return $ maybe False (\credential -> null (missingScopes config credential.scope)) mCredential

-- The access token of the user, refreshed when it expires within a minute; Left with a message for the user otherwise.
-- The credential is locked (within the transaction of the caller), so that concurrent refreshes do not race.
getValidAccessToken :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> OAuthProvider -> m (Either String String)
getValidAccessToken userUuid mWorkspaceUuid provider = do
  mConfig <- resolveProviderConfig mWorkspaceUuid provider
  case mConfig of
    Nothing -> return . Left $ "The connection " ++ providerKey provider ++ " is not configured anymore."
    Just config -> do
      mCredential <- findUserOAuthCredentialForUpdate userUuid (credentialKey provider config)
      case mCredential of
        Nothing -> return . Left $ "Connect your account of " ++ config.name ++ " in your profile first."
        Just credential
          | not (null (missingScopes config credential.scope)) ->
              return . Left $ "Your connection to " ++ config.name ++ " has older permissions, please reconnect it in your profile."
          | otherwise -> do
              now <- liftIO getCurrentTime
              let expiresSoon = maybe False (\expiresAt -> expiresAt <= addUTCTime 60 now) credential.expiresAt
              if expiresSoon
                then refresh config credential
                else Right <$> decrypt credential.accessToken
  where
    expired config = "Your session of " ++ config.name ++ " expired, please reconnect it in your profile."
    refresh config credential =
      case credential.refreshToken of
        Nothing -> return . Left $ expired config
        Just encryptedRefreshToken -> do
          refreshToken <- decrypt encryptedRefreshToken
          eToken <- refreshAccessToken config.tokenUrl config.clientId config.clientSecret refreshToken
          case eToken of
            Right token -> do
              _ <- storeToken userUuid provider config (Just refreshToken) token
              return . Right $ token.accessToken
            Left error
              | "invalid_grant" `L.isInfixOf` error -> do
                  deleteUserOAuthCredential userUuid (credentialKey provider config)
                  return . Left $ expired config
              | otherwise -> return . Left $ error

-- The settings of a provider: Zenodo from the Zenodo settings, a submission definition from the effective submission
-- settings of the workspace; Nothing when it is not configured (anymore)
resolveProviderConfig :: WizardRequestContextC s m => Maybe U.UUID -> OAuthProvider -> m (Maybe OAuthProviderConfig)
resolveProviderConfig _ (ZenodoOAuthProvider repository) = do
  settings <- getCurrentSettingsZenodo
  if repository `elem` availableZenodoRepositories settings
    then do
      let repositorySettings = zenodoRepositorySettings settings repository
      let baseUrl = zenodoBaseUrl repository
      return . Just $
        OAuthProviderConfig
          { name = zenodoRepositoryName repository
          , authorizeUrl = baseUrl ++ "/oauth/authorize"
          , tokenUrl = baseUrl ++ "/oauth/token"
          , clientId = repositorySettings.clientId
          , clientSecret = repositorySettings.clientSecret
          , scopes = zenodoRequiredScopes
          }
    else return Nothing
resolveProviderConfig mWorkspaceUuid (SubmissionOAuthProvider definitionId) = do
  settings <- getEffectiveSettingsSubmission mWorkspaceUuid
  return $ do
    definition <- findDefinition definitionId settings
    oauth <- definitionOAuth definition
    return
      OAuthProviderConfig
        { name = definition.name
        , authorizeUrl = oauth.authorizeUrl
        , tokenUrl = oauth.tokenUrl
        , clientId = oauth.clientId
        , clientSecret = oauth.clientSecret
        , scopes = oauth.scopes
        }

-- The scopes the provider asks for that the token does not have (a token without a scope has what was asked for);
-- OAuth separates the scopes with spaces, some providers with commas
missingScopes :: OAuthProviderConfig -> String -> [String]
missingScopes config scope =
  let granted = words (fmap (\c -> if c == ',' then ' ' else c) scope)
   in filter (`notElem` granted) config.scopes

-- The key a credential is stored with: the provider and the configuration that issued it (its token URL and client),
-- so that a token is never used with another configuration (another server of a workspace override, a changed client)
credentialKey :: OAuthProvider -> OAuthProviderConfig -> String
credentialKey provider config = providerKey provider ++ "#" ++ take 16 (hashSHA256 (config.tokenUrl ++ "\n" ++ config.clientId))

scopeWorkspaceUuid :: WizardRequestContextC s m => m (Maybe U.UUID)
scopeWorkspaceUuid = do
  scope <- asks (.scope')
  return $
    case scope of
      WorkspaceScope workspaceUuid -> Just workspaceUuid
      _ -> Nothing

toConnectionDTO :: OAuthProvider -> OAuthProviderConfig -> Maybe UserOAuthCredential -> OAuthConnectionDTO
toConnectionDTO provider config mCredential =
  OAuthConnectionDTO
    { provider = providerKey provider
    , name = config.name
    , connected = isJust mCredential
    , scope = fmap (.scope) mCredential
    , expiresAt = mCredential >>= (.expiresAt)
    , missingScopes = maybe [] (missingScopes config . (.scope)) mCredential
    }

-- --------------------------------
-- PRIVATE
-- --------------------------------
availableProviders :: WizardRequestContextC s m => Maybe U.UUID -> m [(OAuthProvider, OAuthProviderConfig)]
availableProviders mWorkspaceUuid = do
  zenodo <- availableZenodoRepositories <$> getCurrentSettingsZenodo
  submission <- getEffectiveSettingsSubmission mWorkspaceUuid
  let submissionProviders = [SubmissionOAuthProvider definition.sId | submission.enabled, definition <- submission.definitions, isJust (definitionOAuth definition)]
  let providers = fmap ZenodoOAuthProvider zenodo ++ submissionProviders
  catMaybes <$> forM providers (\provider -> fmap (\config -> (provider, config)) <$> resolveProviderConfig mWorkspaceUuid provider)

requireProviderConfig :: WizardRequestContextC s m => Maybe U.UUID -> OAuthProvider -> m OAuthProviderConfig
requireProviderConfig mWorkspaceUuid provider = do
  mConfig <- resolveProviderConfig mWorkspaceUuid provider
  maybe (throwError . UserError $ _ERROR_SERVICE_OAUTH__PROVIDER_NOT_AVAILABLE (providerKey provider)) return mConfig

getRedirectUri :: WizardRequestContextC s m => m String
getRedirectUri = (++ "/users/edit/current/submission-accounts") <$> getClientUrl

-- Only a path of the client (no other site to redirect to)
safeReturnUrl :: String -> Maybe String
safeReturnUrl url
  | "/" `L.isPrefixOf` url && not ("//" `L.isPrefixOf` url) && '\\' `notElem` url = Just url
  | otherwise = Nothing

storeToken :: WizardRequestContextC s m => U.UUID -> OAuthProvider -> OAuthProviderConfig -> Maybe String -> OAuthTokenResponse -> m UserOAuthCredential
storeToken userUuid provider config mPreviousRefreshToken token = do
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  encryptedAccessToken <- encrypt token.accessToken
  encryptedRefreshToken <- traverse encrypt (maybe mPreviousRefreshToken Just token.refreshToken)
  let credential =
        UserOAuthCredential
          { userUuid = userUuid
          , provider = credentialKey provider config
          , accessToken = encryptedAccessToken
          , refreshToken = encryptedRefreshToken
          , scope = fromMaybe (unwords config.scopes) token.scope
          , expiresAt = fmap (\seconds -> addUTCTime (fromIntegral seconds) now) token.expiresIn
          , tenantUuid = tenantUuid
          , createdAt = now
          , updatedAt = now
          }
  _ <- saveUserOAuthCredential credential
  return credential

encrypt :: WizardRequestContextC s m => String -> m String
encrypt value = do
  serverConfig <- asks (.serverConfig')
  liftIO $ encryptAES256WithRandomIV serverConfig.general.secret value

decrypt :: WizardRequestContextC s m => String -> m String
decrypt value = do
  serverConfig <- asks (.serverConfig')
  return $ decryptAES256WithRandomIV serverConfig.general.secret value
