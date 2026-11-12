module Specs.Api.Handler.DocumentTemplateDraft.File.List_POST (
  list_POST,
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

import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplateFiles
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.Error.Error
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Common (runDraftTemplateMigration)
import Specs.Api.Handler.DocumentTemplateDraft.File.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/document-template-drafts
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/document-template-drafts/{dtUuid}/files" $ do
    test_201 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplate.uuid ++ "/files"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = fileNewFileChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = create_test_201 "HTTP 201 CREATED (user token)" requestContext reqAuthHeader

create_test_201 title requestContext reqAuthHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = fileNewFile
      -- AND: Run migrations
      runDraftTemplateMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateFile)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareTemplateFileDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state
      let templateFileNewFileEdited = fileNewFile {uuid = resDto.uuid} :: DocumentTemplateFile
      assertExistenceOfTemplateFileInDB
        requestContext
        templateFileNewFileEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  create_test_400 "HTTP 400 BAD REQUEST if the name is empty" requestContext ""
  create_test_400 "HTTP 400 BAD REQUEST if the name is blank" requestContext "  "
  create_test_400 "HTTP 400 BAD REQUEST if the name in a folder is empty" requestContext "src/"

create_test_400 title requestContext name =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      let reqBody = encode (fileNewFileChangeDTO {fileName = name} :: DocumentTemplateFileChangeDTO)
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (ValidationError [] (M.singleton "fileName" [_ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_NAME_EMPTY]))
      -- AND: Run migrations
      runDraftTemplateMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      eFiles <- runInContextIO (findFilesByDocumentTemplateUuidAndFileName wizardDocumentTemplate.uuid name) requestContext
      liftIO $ fmap null eFiles `shouldBe` Right True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "documentTemplates.useEditor"
