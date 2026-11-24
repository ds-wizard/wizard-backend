module Shared.Service.Zenodo.ZenodoAuthService (
  getZenodoConnections,
  authorizeZenodo,
  completeZenodoAuthorization,
  disconnectZenodo,
  getValidZenodoAccessToken,
  missingZenodoScopes,
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.Zenodo.ZenodoConnectionDTO
import Shared.Database.DAO.WizardCommon
import Shared.Database.DAO.Zenodo.UserZenodoCredentialDAO
import Shared.Database.DAO.Zenodo.ZenodoOAuthSessionDAO
import Shared.Integration.Http.Zenodo.ZenodoOAuth
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Model.Zenodo.UserZenodoCredential
import Shared.Model.Zenodo.ZenodoOAuthSession
import Shared.Model.Zenodo.ZenodoRepository
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Tenant.TenantHelper (getClientUrl)
import Shared.Util.Crypto

-- The connections of the current user to the Zenodo repositories configured by the administrator
getZenodoConnections :: WizardRequestContextC s m => m [ZenodoConnectionDTO]
getZenodoConnections = do
  user <- getCurrentUser
  settings <- getCurrentSettingsZenodo
  credentials <- findUserZenodoCredentials user.uuid
  return [toConnectionDTO repository (L.find ((== repository) . (.repository)) credentials) | repository <- availableZenodoRepositories settings]

-- Starts the OAuth authorization code flow with PKCE; the frontend redirects the user to the returned URL and Zenodo
-- redirects back to <client url>/zenodo/callback
authorizeZenodo :: WizardRequestContextC s m => ZenodoAuthorizeDTO -> m ZenodoAuthorizationDTO
authorizeZenodo reqDto =
  runInTransaction $ do
    user <- getCurrentUser
    repositorySettings <- getRepositorySettings reqDto.repository
    state <- liftIO $ generateRandomString 32
    codeVerifier <- liftIO $ generateRandomString 48
    encryptedCodeVerifier <- encrypt codeVerifier
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    _ <-
      insertZenodoOAuthSession
        ZenodoOAuthSession
          { state = state
          , userUuid = user.uuid
          , repository = reqDto.repository
          , codeVerifier = encryptedCodeVerifier
          , tenantUuid = tenantUuid
          , createdAt = now
          }
    redirectUri <- getRedirectUri
    let url = buildAuthorizeUrl (zenodoBaseUrl reqDto.repository) repositorySettings.clientId redirectUri (unwords zenodoRequiredScopes) state (pkceCodeChallenge codeVerifier)
    return ZenodoAuthorizationDTO {url = url}

-- Finishes the flow: the state is used only once, it must belong to the current user and be at most 10 minutes old
completeZenodoAuthorization :: WizardRequestContextC s m => ZenodoCallbackDTO -> m ZenodoConnectionDTO
completeZenodoAuthorization reqDto =
  runInTransaction $ do
    user <- getCurrentUser
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    mSession <- takeZenodoOAuthSession reqDto.state
    session <-
      case mSession of
        Just session
          | session.userUuid == user.uuid
              && session.tenantUuid == tenantUuid
              && diffUTCTime now session.createdAt < 600 ->
              return session
        _ -> throwError $ UserError _ERROR_SERVICE_ZENODO__INVALID_STATE
    repositorySettings <- getRepositorySettings session.repository
    codeVerifier <- decrypt session.codeVerifier
    redirectUri <- getRedirectUri
    eToken <- exchangeAuthorizationCode (zenodoBaseUrl session.repository) repositorySettings.clientId repositorySettings.clientSecret redirectUri reqDto.code codeVerifier
    token <- either (throwError . UserError . _ERROR_SERVICE_ZENODO__CONNECTION_FAILED) return eToken
    credential <- storeToken user.uuid session.repository Nothing token
    return $ toConnectionDTO session.repository (Just credential)

disconnectZenodo :: WizardRequestContextC s m => ZenodoRepository -> m ()
disconnectZenodo repository = do
  user <- getCurrentUser
  deleteUserZenodoCredential user.uuid repository

-- The access token of the user, refreshed when it expires within a minute; Left with a message for the user otherwise
getValidZenodoAccessToken :: WizardRequestContextC s m => U.UUID -> ZenodoRepository -> m (Either String String)
getValidZenodoAccessToken userUuid repository = do
  mCredential <- findUserZenodoCredential userUuid repository
  case mCredential of
    Nothing -> return . Left $ "Connect your Zenodo account (" ++ zenodoBaseUrl repository ++ ") in your profile first."
    Just credential
      | not (null (missingZenodoScopes credential.scope)) ->
          return . Left $ "Your Zenodo connection has older permissions, please reconnect it in your profile."
      | otherwise -> do
          now <- liftIO getCurrentTime
          let expiresSoon = maybe False (\expiresAt -> expiresAt <= addUTCTime 60 now) credential.expiresAt
          if expiresSoon
            then refresh credential
            else Right <$> decrypt credential.accessToken
  where
    refresh credential =
      case credential.refreshToken of
        Nothing -> return . Left $ "Your Zenodo session expired, please reconnect it in your profile."
        Just encryptedRefreshToken -> do
          refreshToken <- decrypt encryptedRefreshToken
          settings <- getCurrentSettingsZenodo
          let repositorySettings = zenodoRepositorySettings settings repository
          eToken <- refreshAccessToken (zenodoBaseUrl repository) repositorySettings.clientId repositorySettings.clientSecret refreshToken
          case eToken of
            Right token -> do
              _ <- storeToken userUuid repository (Just refreshToken) token
              return . Right $ token.accessToken
            Left error
              | "invalid_grant" `L.isInfixOf` error -> do
                  deleteUserZenodoCredential userUuid repository
                  return . Left $ "Your Zenodo session expired, please reconnect it in your profile."
              | otherwise -> return . Left $ error

missingZenodoScopes :: String -> [String]
missingZenodoScopes scope = filter (`notElem` words scope) zenodoRequiredScopes

-- --------------------------------
-- PRIVATE
-- --------------------------------
getRepositorySettings :: WizardRequestContextC s m => ZenodoRepository -> m SettingsZenodoRepository
getRepositorySettings repository = do
  settings <- getCurrentSettingsZenodo
  if repository `elem` availableZenodoRepositories settings
    then return $ zenodoRepositorySettings settings repository
    else throwError . UserError $ _ERROR_SERVICE_ZENODO__REPOSITORY_NOT_AVAILABLE (show repository)

getRedirectUri :: WizardRequestContextC s m => m String
getRedirectUri = (++ "/zenodo/callback") <$> getClientUrl

storeToken :: WizardRequestContextC s m => U.UUID -> ZenodoRepository -> Maybe String -> ZenodoTokenResponse -> m UserZenodoCredential
storeToken userUuid repository mPreviousRefreshToken token = do
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  encryptedAccessToken <- encrypt token.accessToken
  encryptedRefreshToken <- traverse encrypt (maybe mPreviousRefreshToken Just token.refreshToken)
  let credential =
        UserZenodoCredential
          { userUuid = userUuid
          , repository = repository
          , accessToken = encryptedAccessToken
          , refreshToken = encryptedRefreshToken
          , scope = fromMaybe (unwords zenodoRequiredScopes) token.scope
          , expiresAt = fmap (\seconds -> addUTCTime (fromIntegral seconds) now) token.expiresIn
          , tenantUuid = tenantUuid
          , createdAt = now
          , updatedAt = now
          }
  _ <- saveUserZenodoCredential credential
  return credential

toConnectionDTO :: ZenodoRepository -> Maybe UserZenodoCredential -> ZenodoConnectionDTO
toConnectionDTO repository mCredential =
  ZenodoConnectionDTO
    { repository = repository
    , baseUrl = zenodoBaseUrl repository
    , connected = maybe False (const True) mCredential
    , scope = fmap (.scope) mCredential
    , expiresAt = mCredential >>= (.expiresAt)
    , missingScopes = maybe [] (missingZenodoScopes . (.scope)) mCredential
    }

encrypt :: WizardRequestContextC s m => String -> m String
encrypt value = do
  serverConfig <- asks (.serverConfig')
  liftIO $ encryptAES256WithRandomIV serverConfig.general.secret value

decrypt :: WizardRequestContextC s m => String -> m String
decrypt value = do
  serverConfig <- asks (.serverConfig')
  return $ decryptAES256WithRandomIV serverConfig.general.secret value
