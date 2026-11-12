module Specs.Service.KnowledgeModel.Editor.Event.EditorEventServiceSpec where

import Data.Time
import Test.Hspec

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.Tenant.Tenant
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper (withEventIdentity)
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService
import qualified Shared.Service.KnowledgeModel.Squash.Squasher as Squasher
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.Uuid

import Specs.Common

editorEventServiceSpec requestContext =
  describe "EditorEventService" $ do
    it "squashEvents squashes an editor of another tenant" $
      -- GIVEN: prepare data
      do
        let differentContext = setTenantUuid differentTenant.uuid requestContext
        let events = [e_km1_ch1_q2, e_km1_ch1_q2_second_edit]
        let editorEvents = fmap (toKnowledgeModelEditorEvent differentKnowledgeModelEditor.uuid differentTenant.uuid) events
        runInContextIO KnowledgeModelPackage.runMigration requestContext
        runInContextIO KnowledgeModelEditor.runMigration requestContext
        runInContextIO (traverse insertKnowledgeModelEvent editorEvents) differentContext
        runInContextIO (updateKnowledgeModelEditorByUuid (differentKnowledgeModelEditor {squashed = False})) differentContext
        -- AND: prepare expectation
        let expEvents = fmap (toKnowledgeModelEditorEvent differentKnowledgeModelEditor.uuid differentTenant.uuid) (Squasher.squash events)
        -- WHEN:
        runInContextIO squashEvents requestContext
        -- THEN:
        (Right resultEvents) <- runInContext (findKnowledgeModelEventsByEditorUuid differentKnowledgeModelEditor.uuid) differentContext
        resultEvents `shouldBe` expEvents
        (Right editor) <- runInContext (findKnowledgeModelEditorByUuid differentKnowledgeModelEditor.uuid) differentContext
        editor.squashed `shouldBe` True
    it "squashEvents keeps the events younger than an hour and the editor not squashed" $
      -- GIVEN: prepare data
      do
        let differentContext = setTenantUuid differentTenant.uuid requestContext
        now <- getCurrentTimeInMicroseconds
        freshUuids <- traverse (const generateUuid) [1 .. 2 :: Int]
        let oldEvents = [e_km1_ch1_q2, e_km1_ch1_q2_second_edit]
        let freshEvents = zipWith3 withEventIdentity freshUuids [addUTCTime (-600) now, addUTCTime (-300) now] oldEvents
        let toEditorEvent = toKnowledgeModelEditorEvent differentKnowledgeModelEditor.uuid differentTenant.uuid
        runInContextIO KnowledgeModelPackage.runMigration requestContext
        runInContextIO KnowledgeModelEditor.runMigration requestContext
        runInContextIO (traverse (insertKnowledgeModelEvent . toEditorEvent) (oldEvents ++ freshEvents)) differentContext
        runInContextIO (updateKnowledgeModelEditorByUuid (differentKnowledgeModelEditor {squashed = False})) differentContext
        -- AND: prepare expectation
        let expEvents = fmap toEditorEvent (Squasher.squash oldEvents ++ freshEvents)
        -- WHEN:
        runInContextIO (squashEventsForEditor differentKnowledgeModelEditor.uuid) differentContext
        -- THEN:
        (Right resultEvents) <- runInContext (findKnowledgeModelEventsByEditorUuid differentKnowledgeModelEditor.uuid) differentContext
        resultEvents `shouldBe` expEvents
        (Right editor) <- runInContext (findKnowledgeModelEditorByUuid differentKnowledgeModelEditor.uuid) differentContext
        editor.squashed `shouldBe` False
