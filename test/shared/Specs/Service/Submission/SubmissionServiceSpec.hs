module Specs.Service.Submission.SubmissionServiceSpec where

import qualified Data.UUID as U
import Database.PostgreSQL.Simple (execute)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.Common (runDB)
import Shared.Database.DAO.Settings.SettingsSubmissionDAO
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import qualified Shared.Database.Migration.Development.Document.DocumentMigration as DOC
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Database.Migration.Development.Submission.Data.Submissions
import qualified Shared.Database.Migration.Development.User.UserMigration as USR
import Shared.Model.Document.Document
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Service.Submission.SubmissionMapper
import Shared.Service.Submission.SubmissionService
import WizardServer.Model.Context.RequestContext

import Specs.Common

submissionServiceSpec requestContext =
  describe "SubmissionService" $ do
    describe "finishSubmission" $ do
      it "sends the submission once all its documents are done" $ do
        prepare requestContext [doc1]
        runInContext' (finishSubmission inProgressSubmission.uuid) requestContext
        submission <- findSubmission requestContext
        submission.state `shouldBe` DoneSubmissionState
      it "does nothing while a document is still generated" $ do
        prepare requestContext [doc1, doc6]
        runInContext' (finishSubmission inProgressSubmission.uuid) requestContext
        submission <- findSubmission requestContext
        submission.state `shouldBe` InProgressSubmissionState
      it "fails the submission if a document could not be generated" $ do
        prepare requestContext [doc1, doc4]
        runInContext' (finishSubmission inProgressSubmission.uuid) requestContext
        submission <- findSubmission requestContext
        submission.state `shouldBe` ErrorSubmissionState
        submission.returnedData `shouldBe` Just ("The document '" ++ doc4.name ++ "' could not be generated")
      it "fails the submission if one of its documents was deleted" $ do
        prepare requestContext [doc1, doc6]
        runInContext' (deleteDocumentByUuid doc6.uuid) requestContext
        runInContext' (finishSubmission inProgressSubmission.uuid) requestContext
        submission <- findSubmission requestContext
        submission.state `shouldBe` ErrorSubmissionState
        submission.returnedData `shouldBe` Just "A document of the submission was deleted"
      it "does nothing if the submission is already finished" $ do
        prepare requestContext [doc4]
        runInContext' (updateSubmissionByUuid (inProgressSubmission {state = DoneSubmissionState} :: Submission)) requestContext
        runInContext' (finishSubmission inProgressSubmission.uuid) requestContext
        submission <- findSubmission requestContext
        submission.state `shouldBe` DoneSubmissionState
    describe "createSubmission" $
      it "refuses a Zenodo submission when the account of the user is not connected" $ do
        prepare requestContext [doc1]
        runInContext' (saveEncryptedSettingsZenodo defaultTenantUuid settingsZenodo) requestContext
        runInContext' (saveSettingsSubmission defaultTenantUuid Nothing (settingsSubmission {definitions = [settingsSubmissionZenodoDefinition]})) requestContext
        result <- runInContext (createSubmission project1.uuid (SubmissionCreateDTO settingsSubmissionZenodoDefinition.sId)) requestContext
        result `shouldBe` Left (UserError (_ERROR_SERVICE_SUBMISSION__NOT_CONNECTED "Zenodo Sandbox"))
    describe "timeOutSubmissions" $
      it "fails the submissions in progress for too long" $ do
        prepare requestContext [doc6]
        runInContext' (timeOutSubmissions 0) requestContext
        submission <- findSubmission requestContext
        submission.state `shouldBe` ErrorSubmissionState
        submission.returnedData `shouldBe` Just "The submission timed out"
    describe "createSubmission (export flags)" $
      it "keeps the maDMP flag of each export with its document" $ do
        prepare requestContext [doc1]
        runInContext' deleteSubmissions requestContext
        runInContext' (saveSettingsSubmission defaultTenantUuid Nothing (settingsSubmission {definitions = [settingsSubmissionDefinition {exports = [settingsSubmissionExport {madmpMetadata = True}]} :: SettingsSubmissionDefinition]})) requestContext
        created <- runInContext' (createSubmission project1.uuid (SubmissionCreateDTO settingsSubmissionDefinition.sId)) requestContext
        submissionDocuments <- runInContext' (findSubmissionDocuments created.uuid) requestContext
        fmap (.madmpMetadata) submissionDocuments `shouldBe` [True]
    describe "document trigger" $
      it "creates a submission command when a document of a submission in progress is finished" $ do
        prepare requestContext [doc6]
        runInContext' deletePersistentCommands requestContext
        runInContext' (runDB (\conn -> execute conn "UPDATE document SET state = 'DoneDocumentState' WHERE uuid = ?" [doc6.uuid])) requestContext
        commands <- runInContext' (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) requestContext
        fmap (\c -> (c.component, c.function, c.createdBy)) commands `shouldBe` [("submission", "submit", inProgressSubmission.createdBy)]

-- --------------------------------
-- HELPERS
-- --------------------------------
inProgressSubmission :: Submission
inProgressSubmission = submission1 {state = InProgressSubmissionState} :: Submission

prepare requestContext documents = do
  runInContext' USR.runMigration requestContext
  runInContext' TML.runMigration requestContext
  runInContext' PRJ.runMigration requestContext
  runInContext' DOC.runMigration requestContext
  runInContext' (mapM_ insertDocument (filter (`notElem` [doc1]) documents)) requestContext
  runInContext' seedSettingsSubmissionDefinition requestContext
  runInContext' deleteSubmissions requestContext
  runInContext' (insertSubmission inProgressSubmission) requestContext
  runInContext' (insertSubmissionDocuments (toSubmissionDocuments inProgressSubmission.uuid inProgressSubmission.tenantUuid (fmap (\d -> (d.uuid, False)) documents))) requestContext

findSubmission requestContext = runInContext' (findSubmissionByUuid inProgressSubmission.uuid) requestContext

runInContext' action requestContext = do
  result <- runInContext action requestContext
  case result of
    Right value -> return value
    Left error -> fail (show error)
