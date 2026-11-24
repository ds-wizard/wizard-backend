module Shared.Service.Settings.WorkspaceSettingsService where

import Control.Applicative ((<|>))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.UUID as U

import Shared.Api.Resource.Settings.SettingsDTO
import qualified Shared.Database.DAO.Settings.SettingsDashboardAndMenuDAO as DashboardAndMenu
import Shared.Database.DAO.Settings.SettingsLookAndFeelDAO
import qualified Shared.Database.DAO.Settings.SettingsProjectsDAO as Projects
import qualified Shared.Database.DAO.Settings.SettingsSubmissionDAO as Submission
import qualified Shared.Database.DAO.Settings.SettingsSupportDAO as Support
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings
import Shared.Model.Workspace.Workspace
import Shared.Service.Project.ProjectValidation
import Shared.Service.Settings.SettingsAuditLog
import Shared.Service.Settings.SettingsService
import Shared.Service.Settings.SettingsSubmissionUtil
import Shared.Util.Crypto (decryptAES256WithRandomIV, encryptAES256WithRandomIV)
import Shared.Util.Secret (maskSecret, unmaskSecret)

dashboardAndMenuSection :: WizardRequestContextC s m => OverridableSettings m SettingsDashboardAndMenu
dashboardAndMenuSection = OverridableSettings "dashboardAndMenu" DashboardAndMenu.table DashboardAndMenu.childTables DashboardAndMenu.findSettingsDashboardAndMenu DashboardAndMenu.saveSettingsDashboardAndMenu (const (return ())) dashboardAndMenuChanges

projectsSection :: WizardRequestContextC s m => OverridableSettings m SettingsProjects
projectsSection = OverridableSettings "projects" Projects.table Projects.childTables Projects.findSettingsProjects Projects.saveSettingsProjects (\settings -> validateProjectTags settings.projectTagging.tags) projectsChanges

supportSection :: WizardRequestContextC s m => OverridableSettings m SettingsSupport
supportSection = OverridableSettings "support" Support.table Support.childTables Support.findSettingsSupport Support.saveSettingsSupport (const (return ())) supportChanges

submissionSection :: WizardRequestContextC s m => OverridableSettings m SettingsSubmission
submissionSection = OverridableSettings "submission" Submission.table Submission.childTables findDecryptedSettingsSubmission saveEncryptedSettingsSubmission (const (return ())) submissionChanges

-- The OAuth client secrets of the submission definitions are stored encrypted and never returned: they show as ********
-- and ******** sent back keeps the stored value (of the same definition, of the organization for a new override)
findDecryptedSettingsSubmission :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m (Maybe SettingsSubmission)
findDecryptedSettingsSubmission tenantUuid mWorkspaceUuid = do
  serverConfig <- asks (.serverConfig')
  let decrypt value = if null value then value else decryptAES256WithRandomIV serverConfig.general.secret value
  fmap (mapOAuthClientSecrets decrypt) <$> Submission.findSettingsSubmission tenantUuid mWorkspaceUuid

saveEncryptedSettingsSubmission :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> SettingsSubmission -> m ()
saveEncryptedSettingsSubmission tenantUuid mWorkspaceUuid settings = do
  serverConfig <- asks (.serverConfig')
  mStored <- findDecryptedSettingsSubmission tenantUuid mWorkspaceUuid
  mOrganization <- findDecryptedSettingsSubmission tenantUuid Nothing
  let stored = mStored <|> mOrganization
  let storedSecret definitionId = maybe "" (\s -> maybe "" (.clientSecret) (findDefinition definitionId s >>= definitionOAuth)) stored
  let unmasked = settings {definitions = fmap (\d -> mapDefinitionOAuth (\o -> o {clientSecret = unmaskSecret (storedSecret d.sId) o.clientSecret}) d) settings.definitions}
  definitions <- traverse (encryptDefinition serverConfig.general.secret) unmasked.definitions
  Submission.saveSettingsSubmission tenantUuid mWorkspaceUuid (unmasked {definitions = definitions})
  where
    encryptDefinition key definition =
      case definitionOAuth definition of
        Just o | not (null o.clientSecret) -> do
          encrypted <- liftIO $ encryptAES256WithRandomIV key o.clientSecret
          return $ mapDefinitionOAuth (\o' -> o' {clientSecret = encrypted}) definition
        _ -> return definition

mapOAuthClientSecrets :: (String -> String) -> SettingsSubmission -> SettingsSubmission
mapOAuthClientSecrets f settings = settings {definitions = fmap (mapDefinitionOAuth (\o -> o {clientSecret = f o.clientSecret})) settings.definitions}

maskSettingsSubmissionDTO :: SettingsDTO SettingsSubmission -> SettingsDTO SettingsSubmission
maskSettingsSubmissionDTO dto = dto {value = mapOAuthClientSecrets maskSecret dto.value} :: SettingsDTO SettingsSubmission

getEffectiveSettingsDashboardAndMenu :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsDashboardAndMenu
getEffectiveSettingsDashboardAndMenu = getEffectiveSettings dashboardAndMenuSection

getEffectiveSettingsProjects :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsProjects
getEffectiveSettingsProjects = getEffectiveSettings projectsSection

getEffectiveSettingsSupport :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsSupport
getEffectiveSettingsSupport = getEffectiveSettings supportSection

getEffectiveSettingsSubmission :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsSubmission
getEffectiveSettingsSubmission = getEffectiveSettings submissionSection

getEffectiveLookAndFeel :: WizardRequestContextC s m => Workspace -> m SettingsLookAndFeel
getEffectiveLookAndFeel workspace = do
  lookAndFeel <- getCurrentSettings findSettingsLookAndFeel
  return (lookAndFeel {logoUrl = workspace.logoUrl <|> lookAndFeel.logoUrl, primaryColor = workspace.primaryColor <|> lookAndFeel.primaryColor} :: SettingsLookAndFeel)

getSettingsDashboardAndMenu :: WizardRequestContextC s m => m (SettingsDTO SettingsDashboardAndMenu)
getSettingsDashboardAndMenu = getOverridableSettings dashboardAndMenuSection

modifySettingsDashboardAndMenu :: WizardRequestContextC s m => SettingsDTO SettingsDashboardAndMenu -> m (SettingsDTO SettingsDashboardAndMenu)
modifySettingsDashboardAndMenu = modifyOverridableSettings dashboardAndMenuSection

deleteSettingsDashboardAndMenu :: WizardRequestContextC s m => m ()
deleteSettingsDashboardAndMenu = deleteOverridableSettings dashboardAndMenuSection

getSettingsProjects :: WizardRequestContextC s m => m (SettingsDTO SettingsProjects)
getSettingsProjects = getOverridableSettings projectsSection

modifySettingsProjects :: WizardRequestContextC s m => SettingsDTO SettingsProjects -> m (SettingsDTO SettingsProjects)
modifySettingsProjects = modifyOverridableSettings projectsSection

deleteSettingsProjects :: WizardRequestContextC s m => m ()
deleteSettingsProjects = deleteOverridableSettings projectsSection

getSettingsSupport :: WizardRequestContextC s m => m (SettingsDTO SettingsSupport)
getSettingsSupport = getOverridableSettings supportSection

modifySettingsSupport :: WizardRequestContextC s m => SettingsDTO SettingsSupport -> m (SettingsDTO SettingsSupport)
modifySettingsSupport = modifyOverridableSettings supportSection

deleteSettingsSupport :: WizardRequestContextC s m => m ()
deleteSettingsSupport = deleteOverridableSettings supportSection

getSettingsSubmission :: WizardRequestContextC s m => m (SettingsDTO SettingsSubmission)
getSettingsSubmission = maskSettingsSubmissionDTO <$> getOverridableSettings submissionSection

modifySettingsSubmission :: WizardRequestContextC s m => SettingsDTO SettingsSubmission -> m (SettingsDTO SettingsSubmission)
modifySettingsSubmission = fmap maskSettingsSubmissionDTO . modifyOverridableSettings submissionSection

deleteSettingsSubmission :: WizardRequestContextC s m => m ()
deleteSettingsSubmission = deleteOverridableSettings submissionSection
