module Specs.Service.Zenodo.ZenodoAuthServiceSpec where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Time
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Zenodo.ZenodoConnectionDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.Settings.SettingsZenodoDAO
import Shared.Database.DAO.Zenodo.UserZenodoCredentialDAO
import Shared.Database.DAO.Zenodo.ZenodoOAuthSessionDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as USR
import Shared.Integration.Http.Zenodo.ZenodoHttp
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Model.Settings.SettingsDM
import Shared.Model.User.User
import Shared.Model.Zenodo.UserZenodoCredential
import Shared.Model.Zenodo.ZenodoOAuthSession
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Zenodo.ZenodoAuthService
import Shared.Util.Crypto
import Shared.Util.Secret (maskedSecret)

import Specs.Common

zenodoAuthServiceSpec requestContext =
  describe "ZenodoAuthService" $ do
    describe "pkceCodeChallenge" $
      it "computes the S256 challenge (RFC 7636, appendix B)" $
        pkceCodeChallenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" `shouldBe` "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
    describe "missingZenodoScopes" $ do
      it "accepts a token with the required scopes" $
        missingZenodoScopes "user:email deposit:write deposit:actions" `shouldBe` []
      it "lists the missing scopes" $
        missingZenodoScopes "deposit:write" `shouldBe` ["deposit:actions"]
    describe "extractZenodoErrorDetail" $ do
      it "flattens the error messages of Zenodo" $
        extractZenodoErrorDetail (BSL.pack "{\"message\":\"A validation error occurred.\",\"errors\":[{\"field\":\"metadata.title\",\"messages\":[\"Missing data for required field.\"]}]}")
          `shouldBe` Just "A validation error occurred.; Missing data for required field."
      it "uses the plain text of a non JSON body" $
        extractZenodoErrorDetail (BSL.pack "Bad   gateway\n") `shouldBe` Just "Bad gateway"
      it "returns nothing for an empty body" $
        extractZenodoErrorDetail (BSL.pack "") `shouldBe` Nothing
    describe "settings" $ do
      it "stores the client secrets encrypted and never returns them" $ do
        prepare requestContext
        dto <- run' (getSettingsZenodo) requestContext
        dto.value.sandbox.clientSecret `shouldBe` maskedSecret
        dto.value.production.clientSecret `shouldBe` ""
        Just stored <- run' (findSettingsZenodo defaultTenantUuid) requestContext
        stored.sandbox.clientSecret `shouldNotBe` settingsZenodo.sandbox.clientSecret
        current <- run' getCurrentSettingsZenodo requestContext
        current `shouldBe` settingsZenodo
      it "keeps the stored secret when the masked value is sent back" $ do
        prepare requestContext
        let reqValue = settingsZenodo {sandbox = settingsZenodo.sandbox {clientId = "new-client-id", clientSecret = maskedSecret} :: SettingsZenodoRepository} :: SettingsZenodo
        _ <- run' (modifySettingsZenodo (SettingsDTO reqValue Nothing Nothing)) requestContext
        current <- run' getCurrentSettingsZenodo requestContext
        current.sandbox.clientId `shouldBe` "new-client-id"
        current.sandbox.clientSecret `shouldBe` settingsZenodo.sandbox.clientSecret
    describe "authorizeZenodo" $ do
      it "creates a session and returns the authorization URL with the PKCE challenge" $ do
        prepare requestContext
        authorization <- run' (authorizeZenodo (ZenodoAuthorizeDTO SandboxZenodoRepository)) requestContext
        "https://sandbox.zenodo.org/oauth/authorize?" `L.isPrefixOf` authorization.url `shouldBe` True
        "client_id=sandbox-client-id" `L.isInfixOf` authorization.url `shouldBe` True
        "code_challenge_method=S256" `L.isInfixOf` authorization.url `shouldBe` True
        "%2Fusers%2Fedit%2Fcurrent%2Fzenodo-connections" `L.isInfixOf` authorization.url `shouldBe` True
      it "refuses a repository that is not configured" $ do
        prepare requestContext
        result <- runInContext (authorizeZenodo (ZenodoAuthorizeDTO ProductionZenodoRepository)) requestContext
        result `shouldBe` Left (UserError (_ERROR_SERVICE_ZENODO__REPOSITORY_NOT_AVAILABLE (show ProductionZenodoRepository)))
    describe "completeZenodoAuthorization" $ do
      it "refuses an unknown state" $ do
        prepare requestContext
        result <- runInContext (completeZenodoAuthorization (ZenodoCallbackDTO "code" "unknown-state")) requestContext
        result `shouldBe` Left (UserError _ERROR_SERVICE_ZENODO__INVALID_STATE)
      it "refuses an expired state (and consumes it)" $ do
        prepare requestContext
        now <- getCurrentTime
        let session = ZenodoOAuthSession "old-state" userAlbert.uuid SandboxZenodoRepository "verifier" defaultTenantUuid (addUTCTime (-3600) now)
        _ <- run' (insertZenodoOAuthSession session) requestContext
        result <- runInContext (completeZenodoAuthorization (ZenodoCallbackDTO "code" "old-state")) requestContext
        result `shouldBe` Left (UserError _ERROR_SERVICE_ZENODO__INVALID_STATE)
    describe "getValidZenodoAccessToken" $ do
      it "asks to connect the account when there is no connection" $ do
        prepare requestContext
        result <- run' (getValidZenodoAccessToken userAlbert.uuid SandboxZenodoRepository) requestContext
        either (const True) (const False) result `shouldBe` True
      it "returns the stored token when it does not expire soon" $ do
        prepare requestContext
        now <- getCurrentTime
        encryptedToken <- encryptAES256WithRandomIV defaultSecret "access-token"
        let credential = UserZenodoCredential userAlbert.uuid SandboxZenodoRepository encryptedToken Nothing "deposit:write deposit:actions" (Just (addUTCTime 3600 now)) defaultTenantUuid now now
        _ <- run' (saveUserZenodoCredential credential) requestContext
        result <- run' (getValidZenodoAccessToken userAlbert.uuid SandboxZenodoRepository) requestContext
        result `shouldBe` Right "access-token"
        connections <- run' getZenodoConnections requestContext
        fmap (\c -> (c.repository, c.connected, null c.missingScopes, isJust c.expiresAt)) connections `shouldBe` [(SandboxZenodoRepository, True, True, True)]

-- --------------------------------
-- HELPERS
-- --------------------------------
prepare requestContext = do
  run' USR.runMigration requestContext
  run' deleteUserZenodoCredentials requestContext
  run' deleteZenodoOAuthSessions requestContext
  run' (saveSettingsZenodo defaultTenantUuid defaultSettingsZenodo) requestContext
  run' (saveEncryptedSettingsZenodo defaultTenantUuid settingsZenodo) requestContext

run' action requestContext = do
  result <- runInContext action requestContext
  case result of
    Right value -> return value
    Left error -> fail (show error)
