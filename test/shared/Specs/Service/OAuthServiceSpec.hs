module Specs.Service.OAuthServiceSpec where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import Data.Time
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.OAuth.OAuthConnectionDTO
import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Submission.SubmissionDefinitionDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.OAuth.OAuthSessionDAO
import Shared.Database.DAO.OAuth.UserOAuthCredentialDAO
import Shared.Database.DAO.Settings.SettingsSubmissionDAO
import Shared.Database.DAO.Settings.SettingsZenodoDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as USR
import Shared.Integration.Http.OAuth.OAuthClient (buildAuthorizeUrl)
import Shared.Integration.Http.Zenodo.ZenodoHttp
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.OAuth.OAuthProvider
import Shared.Model.OAuth.OAuthSession
import Shared.Model.OAuth.UserOAuthCredential
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.Settings.SettingsDM
import Shared.Model.User.User
import Shared.Service.OAuth.OAuthCallbackService
import Shared.Service.OAuth.OAuthService
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Settings.SettingsSubmissionUtil
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Service.Submission.SubmissionService
import Shared.Util.Crypto
import Shared.Util.Secret (maskedSecret)

import Specs.Common

zenodoKey :: String
zenodoKey = providerKey (ZenodoOAuthProvider SandboxZenodoRepository)

oauthServiceSpec requestContext =
  describe "OAuthService" $ do
    describe "pkceCodeChallenge" $
      it "computes the S256 challenge (RFC 7636, appendix B)" $
        pkceCodeChallenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" `shouldBe` "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
    describe "providerKey" $
      it "reads what it writes" $ do
        readProviderKey zenodoKey `shouldBe` Just (ZenodoOAuthProvider SandboxZenodoRepository)
        readProviderKey "submission:oauthRepository" `shouldBe` Just (SubmissionOAuthProvider "oauthRepository")
        readProviderKey "submission:" `shouldBe` Nothing
        readProviderKey "other" `shouldBe` Nothing
    describe "buildAuthorizeUrl" $
      it "keeps the query of the authorize URL" $
        "https://example.org/authorize?tenant=x&response_type=code" `L.isPrefixOf` buildAuthorizeUrl "https://example.org/authorize?tenant=x" "id" "https://dsw/cb" "a b" "state" "challenge" `shouldBe` True
    describe "missingScopes" $ do
      let config = OAuthProviderConfig "Zenodo" "" "" "" "" ["deposit:write", "deposit:actions"]
      it "accepts a token with the required scopes" $
        missingScopes config "user:email deposit:write deposit:actions" `shouldBe` []
      it "lists the missing scopes" $
        missingScopes config "deposit:write" `shouldBe` ["deposit:actions"]
    describe "missingScopes (comma separated)" $
      it "accepts scopes separated by commas" $
        missingScopes (OAuthProviderConfig "GitHub" "" "" "" "" ["repo", "user"]) "repo,user" `shouldBe` []
    describe "extractZenodoErrorDetail" $ do
      it "flattens the error messages of Zenodo" $
        extractZenodoErrorDetail (BSL.pack "{\"message\":\"A validation error occurred.\",\"errors\":[{\"field\":\"metadata.title\",\"messages\":[\"Missing data for required field.\"]}]}")
          `shouldBe` Just "A validation error occurred.; Missing data for required field."
      it "uses the plain text of a non JSON body" $
        extractZenodoErrorDetail (BSL.pack "Bad   gateway\n") `shouldBe` Just "Bad gateway"
      it "returns nothing for an empty body" $
        extractZenodoErrorDetail (BSL.pack "") `shouldBe` Nothing
    describe "Zenodo settings" $ do
      it "stores the client secrets encrypted and never returns them" $ do
        prepare requestContext
        dto <- run' getSettingsZenodo requestContext
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
    describe "submission settings" $ do
      it "stores the OAuth client secret of an HTTP target encrypted and never returns it" $ do
        prepare requestContext
        dto <- run' getSettingsSubmission requestContext
        fmap (fmap (.clientSecret) . definitionOAuth) dto.value.definitions `shouldBe` [Nothing, Just maskedSecret]
        Just stored <- run' (findSettingsSubmission defaultTenantUuid Nothing) requestContext
        fmap (fmap (.clientSecret) . definitionOAuth) stored.definitions `shouldNotBe` [Nothing, Just settingsSubmissionOAuth.clientSecret]
        current <- run' (getEffectiveSettingsSubmission Nothing) requestContext
        current.definitions `shouldBe` [settingsSubmissionDefinition, settingsSubmissionOAuthDefinition]
      it "keeps the stored OAuth client secret when the masked value is sent back" $ do
        prepare requestContext
        dto <- run' getSettingsSubmission requestContext
        _ <- run' (modifySettingsSubmission dto) requestContext
        current <- run' (getEffectiveSettingsSubmission Nothing) requestContext
        current.definitions `shouldBe` [settingsSubmissionDefinition, settingsSubmissionOAuthDefinition]
    describe "getOAuthConnections" $
      it "lists the Zenodo repositories and the HTTP targets with OAuth" $ do
        prepare requestContext
        connections <- run' getOAuthConnections requestContext
        fmap (\c -> (c.provider, c.name, c.connected)) connections
          `shouldBe` [(zenodoKey, "Zenodo Sandbox", False), ("submission:oauthRepository", "OAuth Repository", False)]
    describe "authorizeOAuth" $ do
      it "creates a session and returns the authorization URL of Zenodo with the PKCE challenge" $ do
        prepare requestContext
        authorization <- run' (authorizeOAuth (OAuthAuthorizeDTO zenodoKey Nothing Nothing Nothing)) requestContext
        "https://sandbox.zenodo.org/oauth/authorize?" `L.isPrefixOf` authorization.url `shouldBe` True
        "client_id=sandbox-client-id" `L.isInfixOf` authorization.url `shouldBe` True
        "code_challenge_method=S256" `L.isInfixOf` authorization.url `shouldBe` True
        "%2Fusers%2Fedit%2Fcurrent%2Fsubmission-accounts" `L.isInfixOf` authorization.url `shouldBe` True
      it "returns the authorization URL of an HTTP target and remembers the pending submission" $ do
        prepare requestContext
        authorization <- run' (authorizeOAuth (OAuthAuthorizeDTO "submission:oauthRepository" (Just "/projects/x/documents") (Just project1.uuid) (Just "oauthRepository"))) requestContext
        "https://repository.example.org/oauth/authorize?" `L.isPrefixOf` authorization.url `shouldBe` True
        "scope=deposit" `L.isInfixOf` authorization.url `shouldBe` True
        let state = stateOf authorization.url
        Just session <- run' (takeOAuthSession state userAlbert.uuid defaultTenantUuid) requestContext
        (session.provider, session.returnUrl, session.projectUuid, session.definitionId, session.workspaceUuid)
          `shouldBe` ("submission:oauthRepository", Just "/projects/x/documents", Just project1.uuid, Just "oauthRepository", Just project1.workspaceUuid)
      it "drops a return URL to another site" $ do
        prepare requestContext
        authorization <- run' (authorizeOAuth (OAuthAuthorizeDTO zenodoKey (Just "//evil.example.org") Nothing Nothing)) requestContext
        let state = stateOf authorization.url
        Just session <- run' (takeOAuthSession state userAlbert.uuid defaultTenantUuid) requestContext
        session.returnUrl `shouldBe` Nothing
      it "refuses a provider that is not configured" $ do
        prepare requestContext
        result <- runInContext (authorizeOAuth (OAuthAuthorizeDTO "zenodo:ProductionZenodoRepository" Nothing Nothing Nothing)) requestContext
        result `shouldBe` Left (UserError (_ERROR_SERVICE_OAUTH__PROVIDER_NOT_AVAILABLE "zenodo:ProductionZenodoRepository"))
    describe "completeOAuthAuthorization" $ do
      it "refuses an unknown state" $ do
        prepare requestContext
        result <- runInContext (completeOAuthAuthorization (OAuthCallbackDTO "code" "unknown-state")) requestContext
        result `shouldBe` Left (UserError _ERROR_SERVICE_OAUTH__INVALID_STATE)
      it "refuses an expired state" $ do
        prepare requestContext
        now <- getCurrentTime
        let session = OAuthSession "old-state" userAlbert.uuid zenodoKey Nothing "verifier" Nothing Nothing Nothing defaultTenantUuid (addUTCTime (-3600) now)
        _ <- run' (insertOAuthSession session) requestContext
        result <- runInContext (completeOAuthAuthorization (OAuthCallbackDTO "code" "old-state")) requestContext
        result `shouldBe` Left (UserError _ERROR_SERVICE_OAUTH__INVALID_STATE)
    describe "getValidAccessToken" $ do
      it "asks to connect the account when there is no connection" $ do
        prepare requestContext
        result <- run' (getValidAccessToken userAlbert.uuid Nothing (ZenodoOAuthProvider SandboxZenodoRepository)) requestContext
        either (const True) (const False) result `shouldBe` True
      it "returns the stored token when it does not expire soon" $ do
        prepare requestContext
        connect requestContext (SubmissionOAuthProvider "oauthRepository") "deposit"
        result <- run' (getValidAccessToken userAlbert.uuid Nothing (SubmissionOAuthProvider "oauthRepository")) requestContext
        result `shouldBe` Right "access-token"
      it "asks to reconnect a connection with older permissions" $ do
        prepare requestContext
        connect requestContext (ZenodoOAuthProvider SandboxZenodoRepository) "deposit:write"
        result <- run' (getValidAccessToken userAlbert.uuid Nothing (ZenodoOAuthProvider SandboxZenodoRepository)) requestContext
        either (const True) (const False) result `shouldBe` True
    describe "credentialKey" $
      it "does not use a token issued by another configuration of the provider" $ do
        prepare requestContext
        connect requestContext (SubmissionOAuthProvider "oauthRepository") "deposit"
        Just config <- run' (resolveProviderConfig Nothing (SubmissionOAuthProvider "oauthRepository")) requestContext
        connectedBefore <- run' (isConnected userAlbert.uuid (SubmissionOAuthProvider "oauthRepository") config) requestContext
        connectedBefore `shouldBe` True
        let otherServer = config {tokenUrl = "https://other.example.org/oauth/token"} :: OAuthProviderConfig
        connectedAfter <- run' (isConnected userAlbert.uuid (SubmissionOAuthProvider "oauthRepository") otherServer) requestContext
        connectedAfter `shouldBe` False
    describe "disconnectOAuth" $
      it "removes the connections of the provider" $ do
        prepare requestContext
        connect requestContext (SubmissionOAuthProvider "oauthRepository") "deposit"
        run' (disconnectOAuth "submission:oauthRepository") requestContext
        connections <- run' getOAuthConnections requestContext
        fmap (.connected) connections `shouldBe` [False, False]
    describe "takeOAuthSession" $
      it "is taken only by the user who started it" $ do
        prepare requestContext
        now <- getCurrentTime
        let session = OAuthSession "someone-elses-state" userAlbert.uuid zenodoKey Nothing "verifier" Nothing Nothing Nothing defaultTenantUuid now
        _ <- run' (insertOAuthSession session) requestContext
        taken <- run' (takeOAuthSession "someone-elses-state" userNikola.uuid defaultTenantUuid) requestContext
        taken `shouldBe` Nothing
        takenByOwner <- run' (takeOAuthSession "someone-elses-state" userAlbert.uuid defaultTenantUuid) requestContext
        fmap (.state) takenByOwner `shouldBe` Just "someone-elses-state"
    describe "getAvailableSubmissions" $
      it "says which submissions need a connection of the user" $ do
        prepare requestContext
        connect requestContext (ZenodoOAuthProvider SandboxZenodoRepository) "deposit:write deposit:actions"
        run' (saveEncryptedSettingsSubmission defaultTenantUuid Nothing (settingsSubmission {definitions = [settingsSubmissionDefinition, settingsSubmissionOAuthDefinition, settingsSubmissionZenodoDefinition]})) requestContext
        definitions <- run' (getAvailableSubmissions project1.uuid) requestContext
        fmap (\d -> (d.sId, d.oauthProvider, d.requiresConnection)) definitions
          `shouldBe` [ (settingsSubmissionDefinition.sId, Nothing, False)
                     , ("oauthRepository", Just "submission:oauthRepository", True)
                     , (settingsSubmissionZenodoDefinition.sId, Just zenodoKey, False)
                     ]

-- --------------------------------
-- HELPERS
-- --------------------------------
prepare requestContext = do
  run' USR.runMigration requestContext
  run' TML.runMigration requestContext
  run' PRJ.runMigration requestContext
  run' deleteUserOAuthCredentials requestContext
  run' deleteOAuthSessions requestContext
  run' (saveSettingsZenodo defaultTenantUuid defaultSettingsZenodo) requestContext
  run' (saveEncryptedSettingsZenodo defaultTenantUuid settingsZenodo) requestContext
  run' (saveEncryptedSettingsSubmission defaultTenantUuid Nothing (settingsSubmission {definitions = [settingsSubmissionDefinition, settingsSubmissionOAuthDefinition]})) requestContext

-- Stores a connection of Albert issued by the current configuration of the provider
connect requestContext provider scope = do
  Just config <- run' (resolveProviderConfig Nothing provider) requestContext
  now <- getCurrentTime
  encryptedToken <- encryptAES256WithRandomIV defaultSecret "access-token"
  let credential = UserOAuthCredential userAlbert.uuid (credentialKey provider config) encryptedToken Nothing scope (Just (addUTCTime 3600 now)) defaultTenantUuid now now
  _ <- run' (saveUserOAuthCredential credential) requestContext
  return ()

-- The state parameter of an authorization URL
stateOf :: String -> String
stateOf url =
  case [drop (length "state=") parameter | parameter <- splitOn '&' (drop 1 (dropWhile (/= '?') url)), "state=" `L.isPrefixOf` parameter] of
    (state : _) -> state
    [] -> ""
  where
    splitOn separator text =
      case break (== separator) text of
        (part, []) -> [part]
        (part, _ : rest) -> part : splitOn separator rest

run' action requestContext = do
  result <- runInContext action requestContext
  case result of
    Right value -> return value
    Left error -> fail (show error)
