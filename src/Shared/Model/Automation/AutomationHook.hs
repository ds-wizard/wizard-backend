module Shared.Model.Automation.AutomationHook where

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Project

data AutomationHook
  = ProjectCreateHook Project
  | ProjectSubmitHook Project String
  | KnowledgeModelPublishHook KnowledgeModelPackage
  deriving (Show)

data AutomationHookDecision
  = AllowAutomationHookDecision {tags :: [String]}
  | DenyAutomationHookDecision {reason :: String}
  deriving (Show, Eq)

allowAutomationHook :: AutomationHookDecision
allowAutomationHook = AllowAutomationHookDecision {tags = []}
