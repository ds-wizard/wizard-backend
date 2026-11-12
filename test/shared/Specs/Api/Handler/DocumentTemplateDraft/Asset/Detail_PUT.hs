module Specs.Api.Handler.DocumentTemplateDraft.Asset.Detail_PUT (
  detail_PUT,
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

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplateAssets
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.Error.Error
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Asset.Common
import Specs.Api.Handler.DocumentTemplateDraft.Common (runDraftTemplateMigration)

-- ------------------------------------------------------------------------
-- PUT /api/document-template-drafts/{dtUuid}/assets/{assetUuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /api/document-template-drafts/{dtUuid}/assets/{assetUuid}" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplate.uuid ++ "/assets/" ++ U.toString assetLogo.uuid

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = assetLogoChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = assetLogoEdited
    -- AND: Run migrations
    runDraftTemplateMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateAsset)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfTemplateAssetInDB requestContext assetLogoEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST if the name is empty" $ do
    -- GIVEN: Prepare request
    let reqBody = encode (assetLogoChangeDTO {fileName = ""} :: DocumentTemplateAssetChangeDTO)
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
    assertExistenceOfTemplateAssetInDB requestContext assetLogo

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "documentTemplates.useEditor"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    (BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplate.uuid ++ "/assets/b918fa4a-b0cf-4bef-ada6-e8cb3848cc75")
    reqHeaders
    reqBody
    "document_template_asset"
    [("uuid", "b918fa4a-b0cf-4bef-ada6-e8cb3848cc75")]
