module Shared.Model.Automation.AutomationEvent where

import Data.List (find, sort)
import Data.Time
import qualified Data.UUID as U

import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Submission.Submission
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.Workspace.Workspace

data AutomationEvent
  = ProjectCreatedEvent Project ProjectSource
  | ProjectUpdatedEvent Project [String]
  | ProjectDeletedEvent Project
  | ProjectMigratedEvent Project U.UUID U.UUID
  | ProjectVersionCreatedEvent Project ProjectVersion
  | ProjectPermissionChangedEvent Project ProjectPerm ProjectPermissionChange
  | ProjectFileUploadedEvent Project ProjectFile
  | ProjectFileDeletedEvent Project ProjectFile
  | DocumentCreatedEvent Document
  | DocumentDeletedEvent Document
  | ProjectSubmittedEvent Project Submission
  | ProjectSubmissionFailedEvent Project Submission (Maybe String)
  | KnowledgeModelPublishedEvent KnowledgeModelPackage PackageSource
  | KnowledgeModelDeprecatedEvent KnowledgeModelPackage
  | KnowledgeModelDeletedEvent KnowledgeModelPackage
  | DocumentTemplatePublishedEvent DocumentTemplate DocumentTemplateSource
  | DocumentTemplateDeprecatedEvent DocumentTemplate
  | DocumentTemplateDeletedEvent DocumentTemplate
  | UserCreatedEvent User UserSource
  | UserUpdatedEvent User [String]
  | UserDeletedEvent User
  | UserGroupMemberAddedEvent UserGroup U.UUID
  | UserGroupMemberRemovedEvent UserGroup U.UUID
  | WorkspaceCreatedEvent Workspace
  | WorkspaceDeletedEvent Workspace
  | WorkspaceMemberAddedEvent U.UUID U.UUID U.UUID
  | WorkspaceMemberRemovedEvent U.UUID U.UUID
  | AnalyticsSynchronizedEvent AnalyticsSynchronization
  deriving (Show)

data ProjectSource
  = NewProjectSource
  | TemplateProjectSource
  | CloneProjectSource
  | ImportProjectSource
  deriving (Show, Eq, Enum, Bounded)

data PackageSource
  = EditorPackageSource
  | FilePackageSource
  | RegistryPackageSource
  deriving (Show, Eq, Enum, Bounded)

data DocumentTemplateSource
  = EditorDocumentTemplateSource
  | FileDocumentTemplateSource
  | RegistryDocumentTemplateSource
  deriving (Show, Eq, Enum, Bounded)

data UserSource
  = RegistrationUserSource
  | InvitationUserSource
  | ExternalUserSource
  deriving (Show, Eq, Enum, Bounded)

data ProjectPermissionChange
  = AddedProjectPermissionChange
  | ChangedProjectPermissionChange
  | RemovedProjectPermissionChange
  deriving (Show, Eq, Enum, Bounded)

data AnalyticsSynchronization = AnalyticsSynchronization
  { uuid :: U.UUID
  , state :: String
  , startedAt :: UTCTime
  , finishedAt :: Maybe UTCTime
  }
  deriving (Show, Eq)

projectChangedFields :: Project -> Project -> [String]
projectChangedFields old new =
  [ field
  | (field, changed) <-
      [ ("name", old.name /= new.name)
      , ("description", old.description /= new.description)
      , ("visibility", old.visibility /= new.visibility)
      , ("sharing", old.sharing /= new.sharing)
      , ("tags", old.projectTags /= new.projectTags)
      , ("documentTemplate", old.documentTemplateUuid /= new.documentTemplateUuid || old.formatUuid /= new.formatUuid)
      , ("isTemplate", old.isTemplate /= new.isTemplate)
      ]
  , changed
  ]

userChangedFields :: User -> User -> [String]
userChangedFields old new =
  [ field
  | (field, changed) <-
      [ ("firstName", old.firstName /= new.firstName)
      , ("lastName", old.lastName /= new.lastName)
      , ("email", old.email /= new.email)
      , ("affiliation", old.affiliation /= new.affiliation)
      , ("role", old.role.uuid /= new.role.uuid)
      , ("active", old.active /= new.active)
      ]
  , changed
  ]

projectPermissionChanges :: Project -> Project -> [(ProjectPerm, ProjectPermissionChange)]
projectPermissionChanges old new =
  [(perm, AddedProjectPermissionChange) | perm <- new.permissions, perm.memberUuid `notElem` oldMembers]
    ++ [(perm, ChangedProjectPermissionChange) | perm <- new.permissions, Just oldPerm <- [find ((== perm.memberUuid) . (.memberUuid)) old.permissions], sort oldPerm.perms /= sort perm.perms]
    ++ [(perm, RemovedProjectPermissionChange) | perm <- old.permissions, perm.memberUuid `notElem` newMembers]
  where
    oldMembers = fmap (.memberUuid) old.permissions
    newMembers = fmap (.memberUuid) new.permissions
