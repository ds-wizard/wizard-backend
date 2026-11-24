module Shared.Database.Migration.Development.Settings.Data.Settings where

import qualified Data.Map.Strict as M

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.Common.SensitiveData
import Shared.Model.Config.SimpleFeature
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.Settings.SettingsDM
import Shared.Model.Settings.SettingsEM ()
import Shared.Util.Uuid

defaultSecret = "01234567890123456789012345678901"

settingsAuthentication :: SettingsAuthentication
settingsAuthentication = defaultSettingsAuthentication

settingsUsers :: SettingsUsers
settingsUsers = defaultSettingsUsers

settingsRoles :: SettingsRoles
settingsRoles = SettingsRoles {defaultRoleUuid = u' "4e79c242-4100-4447-becd-45eca47fa60b"}

settingsAnnouncement :: SettingsAnnouncement
settingsAnnouncement = SettingsAnnouncement {content = "Hello", level = InfoAnnouncementLevelType}

settingsLoginScreen :: SettingsLoginScreen
settingsLoginScreen = defaultSettingsLoginScreen {announcements = [settingsAnnouncement]}

settingsCustomMenuLink :: SettingsCustomMenuLink
settingsCustomMenuLink =
  SettingsCustomMenuLink
    { icon = "faq"
    , title = "My Link"
    , url = "http://example.prg"
    , newWindow = False
    }

settingsDashboardAndMenu :: SettingsDashboardAndMenu
settingsDashboardAndMenu =
  SettingsDashboardAndMenu
    { customMenuLinks = [settingsCustomMenuLink]
    , announcements = [settingsAnnouncement]
    }

settingsRegistry :: SettingsRegistry
settingsRegistry = SettingsRegistry {enabled = True, apiKey = "GlobalApiKey"}

settingsRegistryEncrypted :: SettingsRegistry
settingsRegistryEncrypted = process defaultSecret settingsRegistry

settingsFeatures :: SettingsFeatures
settingsFeatures = defaultSettingsFeatures

settingsLookAndFeel :: SettingsLookAndFeel
settingsLookAndFeel = defaultSettingsLookAndFeel

_SETTINGS__PROJECT_TAG_1 = "settingsProjectTag1"

_SETTINGS__PROJECT_TAG_2 = "settingsProjectTag2"

settingsProjects :: SettingsProjects
settingsProjects =
  SettingsProjects
    { projectVisibility = SettingsProjectsVisibility {enabled = True, defaultValue = PrivateProjectVisibility}
    , projectSharing = SettingsProjectsSharing {enabled = True, defaultValue = RestrictedProjectSharing, anonymousEnabled = False}
    , projectCreation = TemplateAndCustomProjectCreation
    , projectTagging = SettingsProjectsTagging {enabled = True, tags = [_SETTINGS__PROJECT_TAG_1, _SETTINGS__PROJECT_TAG_2]}
    , summaryReport = SimpleFeature True
    }

editedSettingsProjects :: SettingsProjects
editedSettingsProjects = settingsProjects {summaryReport = SimpleFeature False}

settingsSupport :: SettingsSupport
settingsSupport = defaultSettingsSupport

editedSettingsSupport :: SettingsSupport
editedSettingsSupport = settingsSupport {supportEmail = Just "support@example.com"}

settingsSubmission :: SettingsSubmission
settingsSubmission = SettingsSubmission {enabled = True, definitions = [settingsSubmissionDefinition]}

settingsSubmissionEmpty :: SettingsSubmission
settingsSubmissionEmpty = SettingsSubmission {enabled = True, definitions = []}

settingsSubmissionServiceApiTokenProp :: String
settingsSubmissionServiceApiTokenProp = "API Token"

settingsSubmissionServiceSecretProp :: String
settingsSubmissionServiceSecretProp = "Secret"

settingsSubmissionDefinition :: SettingsSubmissionDefinition
settingsSubmissionDefinition =
  SettingsSubmissionDefinition
    { sId = "mySubmissionServer"
    , name = "My Submission Server"
    , description = "Some description"
    , exports = [settingsSubmissionExport]
    , target = settingsSubmissionHttpTarget
    }

settingsSubmissionExport :: SettingsSubmissionExport
settingsSubmissionExport =
  SettingsSubmissionExport
    { id = wizardDocumentTemplate.id
    , version = wizardDocumentTemplate.version
    , formatName = formatJson.name
    , madmpMetadata = False
    }

settingsSubmissionHttpTarget :: SettingsSubmissionTarget
settingsSubmissionHttpTarget =
  HttpSubmissionTarget
    { props = [settingsSubmissionServiceApiTokenProp, settingsSubmissionServiceSecretProp]
    , request = settingsSubmissionServiceRequest
    }

settingsSubmissionZenodoDefinition :: SettingsSubmissionDefinition
settingsSubmissionZenodoDefinition =
  SettingsSubmissionDefinition
    { sId = "zenodoSandbox"
    , name = "Zenodo Sandbox"
    , description = "Deposit the DMP to Zenodo Sandbox"
    , exports = [settingsSubmissionExport {madmpMetadata = True}]
    , target =
        ZenodoSubmissionTarget
          { repository = SandboxZenodoRepository
          , resourceType = "publication-datamanagementplan"
          , licenseId = "cc-by-4.0"
          , accessLevel = "public"
          , community = Nothing
          }
    }

settingsSubmissionServiceRequest :: SettingsSubmissionServiceRequest
settingsSubmissionServiceRequest =
  SettingsSubmissionServiceRequest
    { method = "GET"
    , url = "https://mockserver.ds-wizard.org/submission.json"
    , headers = M.fromList [("Api-Key", "${API Token}")]
    , multipart = SettingsSubmissionServiceRequestMultipart {enabled = False, fileName = "file"}
    }
