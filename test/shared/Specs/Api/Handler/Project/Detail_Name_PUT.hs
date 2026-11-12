module Specs.Api.Handler.Project.Detail_Name_PUT (
  detail_name_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.ProjectNameChangeDTO
import Shared.Api.Resource.Project.ProjectNameChangeJM ()
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/projects/{projectUuid}/name
-- ------------------------------------------------------------------------
detail_name_PUT :: RequestContext -> SpecWith ((), Application)
detail_name_PUT requestContext =
  describe "PUT /api/projects/{projectUuid}/name" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT projectUuid = BS.pack $ "/api/projects/" ++ U.toString projectUuid ++ "/name"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT project = ProjectNameChangeDTO {name = project.name}

reqBodyT project = encode $ reqDtoT project

renamed project = project {name = "EDITED: " ++ project.name} :: Project

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" requestContext project1 project1Events [reqAuthHeader] False
  create_test_200 "HTTP 200 OK (Owner, VisibleView)" requestContext project2 project2Events [reqAuthHeader] False
  create_test_200
    "HTTP 200 OK (Non-Owner, Private, Sharing, Anonymous Enabled)"
    requestContext
    project10
    project10Events
    [reqNonAdminAuthHeader]
    True

create_test_200 title requestContext project projectEvents authHeader anonymousEnabled =
  it title $
    -- GIVEN: Prepare request
    do
      let projectEdited = renamed project
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT projectEdited
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = reqDtoT projectEdited
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO (insertProjectEvents project10Events) requestContext
      -- AND: Enabled anonymous sharing
      updateAnonymousProjectSharing requestContext anonymousEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB requestContext projectEdited projectEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod (reqUrlT project1.uuid) "name"
  create_test_400 "HTTP 400 BAD REQUEST when the name is empty" requestContext ""
  create_test_400 "HTTP 400 BAD REQUEST when the name is blank" requestContext "   "

create_test_400 title requestContext reqName =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project1.uuid
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = encode $ ProjectNameChangeDTO {name = reqName}
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "name" [_ERROR_VALIDATION__FIELDS_ABSENCE])
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB requestContext project1 project1Events

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext =
  createAuthTest reqMethod (reqUrlT project3.uuid) [reqCtHeader] (reqBodyT project1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" requestContext project1 project1Events
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" requestContext project2 project2Events
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private, Sharing, Anonymous Disabled)" requestContext project10 project10Events

create_test_403 title requestContext project projectEvents =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT [reqNonAdminAuthHeader]
      let reqBody = reqBodyT (renamed project)
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Administrate Project"
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO (insertProjectEvents project10Events) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB requestContext project projectEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/name"
    (reqHeadersT [reqAuthHeader])
    (reqBodyT project1)
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
