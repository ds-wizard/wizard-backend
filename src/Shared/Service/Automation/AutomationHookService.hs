module Shared.Service.Automation.AutomationHookService where

import Control.Monad.Except (throwError)
import Data.List (nub)

import Shared.Localization.Messages.Public
import Shared.Model.Automation.AutomationHook
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Project.Project

checkAutomationHook :: WizardRequestContextC s m => AutomationHook -> m [String]
checkAutomationHook hook = do
  decision <- runAutomationHook hook
  case decision of
    AllowAutomationHookDecision {..} -> return tags
    DenyAutomationHookDecision {..} -> throwError . UserError $ _ERROR_SERVICE_AUTOMATION__HOOK_DENIED reason

checkProjectCreateHook :: WizardRequestContextC s m => Project -> m Project
checkProjectCreateHook project = do
  tags <- checkAutomationHook (ProjectCreateHook project)
  return project {projectTags = nub (project.projectTags ++ tags)}
