module Specs.Api.Handler.Submission.List_Available_GET (
  list_available_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Submission.SubmissionDefinitionJM ()
import Shared.Constant.Tenant
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Settings.SettingsSubmissionDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Database.Migration.Development.Submission.Data.Submissions
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/projects/{projectUuid}/available-submissions
-- ------------------------------------------------------------------------
list_available_GET :: RequestContext -> SpecWith ((), Application)
list_available_GET requestContext =
  describe "GET /api/projects/{projectUuid}/available-submissions" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT project = BS.pack $ "/api/projects/" ++ U.toString project.uuid ++ "/available-submissions"

reqUrl = reqUrlT project1

reqHeaders = [reqAuthHeader]

reqBody = ""

runMigrations requestContext = do
  runInContextIO U_Migration.runMigration requestContext
  runInContextIO TML_Migration.runMigration requestContext
  runInContextIO PRJ_Migration.runMigration requestContext
  runInContextIO (insertProject project10) requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode [settingsSubmissionDefinitionDto]
      -- AND: Run migrations
      runMigrations requestContext
      runInContextIO seedSettingsSubmissionDefinition requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK (definitions with an unknown format or without exports are not offered)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let unknownFormat = settingsSubmissionDefinition {sId = "unknownFormat", exports = [settingsSubmissionExport {formatName = "Other format"}]} :: SettingsSubmissionDefinition
      let noExports = settingsSubmissionDefinition {sId = "noExports", exports = []} :: SettingsSubmissionDefinition
      let expBody = encode [settingsSubmissionDefinitionDto]
      -- AND: Run migrations
      runMigrations requestContext
      runInContextIO (saveSettingsSubmission defaultTenantUuid Nothing (settingsSubmission {definitions = [settingsSubmissionDefinition, unknownFormat, noExports]})) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (ForbiddenError (_ERROR_VALIDATION__FORBIDDEN "Edit Project"))
      -- AND: Run migrations
      runMigrations requestContext
      runInContextIO seedSettingsSubmissionDefinition requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project2) [reqNonAdminAuthHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
