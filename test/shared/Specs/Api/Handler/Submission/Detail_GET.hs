module Specs.Api.Handler.Submission.Detail_GET (
  detail_GET,
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
import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.Submission.Data.Submissions
import qualified Shared.Database.Migration.Development.Submission.SubmissionMigration as SUB_Migration
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Submission.Submission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/projects/{projectUuid}/submissions/{submissionUuid}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/projects/{projectUuid}/submissions/{submissionUuid}" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT project = BS.pack $ "/api/projects/" ++ U.toString project.uuid ++ "/submissions/" ++ U.toString submission1.uuid

reqUrl = reqUrlT project1

reqHeaders = [reqAuthHeader]

reqBody = ""

runMigrations requestContext = do
  runInContextIO U_Migration.runMigration requestContext
  runInContextIO TML_Migration.runMigration requestContext
  runInContextIO PRJ_Migration.runMigration requestContext
  runInContextIO (insertProject project10) requestContext
  runInContextIO DOC_Migration.runMigration requestContext
  runInContextIO SUB_Migration.runMigration requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode submission1List
      -- AND: Run migrations
      runMigrations requestContext
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
  it "HTTP 403 FORBIDDEN (Non-Owner, Private)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (ForbiddenError (_ERROR_VALIDATION__FORBIDDEN "View Project"))
      -- AND: Run migrations
      runMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl [reqNonAdminAuthHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  it "HTTP 404 NOT FOUND (the submission belongs to another project)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "submission" [("uuid", U.toString submission1.uuid)]))
      -- AND: Run migrations
      runMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project2) reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
