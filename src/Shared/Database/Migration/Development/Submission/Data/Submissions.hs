module Shared.Database.Migration.Development.Submission.Data.Submissions where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Api.Resource.Submission.SubmissionDefinitionDTO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Service.Submission.SubmissionMapper
import Shared.Util.Uuid

submissionCreate :: SubmissionCreateDTO
submissionCreate = SubmissionCreateDTO {definitionId = settingsSubmissionDefinition.sId}

settingsSubmissionDefinitionDto :: SubmissionDefinitionDTO
settingsSubmissionDefinitionDto =
  SubmissionDefinitionDTO
    { sId = settingsSubmissionDefinition.sId
    , name = settingsSubmissionDefinition.name
    , description = settingsSubmissionDefinition.description
    , targetType = "HttpSubmissionTarget"
    , exports =
        [ SubmissionDefinitionExportDTO
            { documentTemplateUuid = wizardDocumentTemplate.uuid
            , documentTemplateName = wizardDocumentTemplate.name
            , documentTemplateId = wizardDocumentTemplate.id
            , documentTemplateVersion = wizardDocumentTemplate.version
            , formatUuid = formatJson.uuid
            , formatName = formatJson.name
            }
        ]
    , oauthProvider = Nothing
    , requiresConnection = False
    , connectOnSubmit = True
    }

submission1 :: Submission
submission1 =
  Submission
    { uuid = u' "f9e14cfb-4435-45a5-8a10-2ea5dbcb92b0"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , definitionId = settingsSubmissionDefinition.sId
    , projectUuid = project1.uuid
    , externalId = Nothing
    , targetBaseUrl = Nothing
    , tenantUuid = defaultTenant.uuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission1Documents :: [SubmissionDocument]
submission1Documents = [SubmissionDocument {submissionUuid = submission1.uuid, position = 0, documentUuid = Just doc1.uuid, madmpMetadata = False, tenantUuid = defaultTenant.uuid}]

submission1List :: SubmissionList
submission1List = toList submission1 (Just settingsSubmissionDefinition) [doc1] (Just userAlbertSuggestion)

differentSubmission1 :: Submission
differentSubmission1 =
  Submission
    { uuid = u' "de51c280-2a6f-49d0-b3de-405401ffba74"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , definitionId = settingsSubmissionDefinition.sId
    , projectUuid = differentProject.uuid
    , externalId = Nothing
    , targetBaseUrl = Nothing
    , tenantUuid = differentTenant.uuid
    , createdBy = Just userCharles.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

differentSubmission1Documents :: [SubmissionDocument]
differentSubmission1Documents = [SubmissionDocument {submissionUuid = differentSubmission1.uuid, position = 0, documentUuid = Just differentDoc.uuid, madmpMetadata = False, tenantUuid = differentTenant.uuid}]
