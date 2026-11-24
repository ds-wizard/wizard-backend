module Shared.Service.Document.DocumentUtil where

import qualified Data.UUID as U

import Shared.Api.Resource.Document.DocumentDTO
import Shared.Model.Common.Page
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentList
import Shared.Service.Document.DocumentMapper

enhanceDocuments :: WizardRequestContextC s m => Page DocumentList -> m (Page DocumentDTO)
enhanceDocuments = return . fmap toDTO

matchesDocumentKey :: String -> U.UUID -> U.UUID -> Maybe String -> Document -> Bool
matchesDocumentKey name documentTemplateUuid formatUuid mLanguage doc =
  doc.name == name
    && doc.documentTemplateUuid == documentTemplateUuid
    && doc.formatUuid == formatUuid
    && doc.language == mLanguage

isFinishedDocument :: Document -> Bool
isFinishedDocument doc = doc.state == DoneDocumentState || doc.state == ErrorDocumentState

isPendingDocument :: Document -> Bool
isPendingDocument doc = doc.state == QueuedDocumentState || doc.state == InProgressDocumentState
