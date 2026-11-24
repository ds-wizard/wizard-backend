module Specs.Api.Handler.Submission.List_POST (
  list_POST,
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
import Shared.Api.Resource.Submission.SubmissionCreateJM ()
import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Submission.SubmissionDAO
import qualified Shared.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Database.Migration.Development.Submission.Data.Submissions
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Service.Submission.SubmissionMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/projects/{projectUuid}/submissions
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/projects/{projectUuid}/submissions" $ do
    test_201 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT project = BS.pack $ "/api/projects/" ++ U.toString project.uuid ++ "/submissions"

reqUrl = reqUrlT project1

reqHeadersT authHeader = reqCtHeader : authHeader

reqDto = submissionCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 "HTTP 201 CREATED (Owner, Private)" requestContext project1 [reqAuthHeader] userAlbertSuggestion
  create_test_201
    "HTTP 201 CREATED (Non-Owner, VisibleEdit)"
    requestContext
    project3
    [reqNonAdminAuthHeader]
    userNikolaSuggestionDto

create_test_201 title requestContext project authHeader user =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = (toList submission1 (Just settingsSubmissionDefinition) [] (Just user)) {state = InProgressSubmissionState, projectUuid = project.uuid} :: SubmissionList
      let expBody = encode expDto
      let expType (a :: SubmissionList) = a
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      runInContextIO seedSettingsSubmissionDefinition requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project) reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["uuid", "createdAt", "updatedAt", "documents"]
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findSubmissions requestContext 1
      submissions <- getOneFromDB findSubmissions requestContext
      documentUuids <- getOneFromDB (findSubmissionDocumentUuids (head submissions).uuid) requestContext
      liftIO $ length documentUuids `shouldBe` 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST if the submission is not available for the project" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = encode (submissionCreate {definitionId = "otherSubmission"})
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError (_ERROR_SERVICE_SUBMISSION__DEFINITION_NOT_AVAILABLE "otherSubmission")
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      runInContextIO seedSettingsSubmissionDefinition requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findSubmissions requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    requestContext
    project2
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")

create_test_403 title requestContext project authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO (insertProject project7) requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      runInContextIO seedSettingsSubmissionDefinition requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project) reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
