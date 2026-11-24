module Specs.Service.Mcp.Tool.DocumentTools.ToolSpec (
  documentToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentTools.CreateDocument
import Specs.Service.Mcp.Tool.DocumentTools.DeleteDocument
import Specs.Service.Mcp.Tool.DocumentTools.DownloadDocument
import Specs.Service.Mcp.Tool.DocumentTools.ListAvailableSubmissions
import Specs.Service.Mcp.Tool.DocumentTools.ListDocuments
import Specs.Service.Mcp.Tool.DocumentTools.SubmitProject

documentToolsSpec :: RequestContext -> SpecWith ((), Application)
documentToolsSpec requestContext = do
  list_documents requestContext
  create_document requestContext
  download_document requestContext
  delete_document requestContext
  list_available_submissions requestContext
  submit_project requestContext
