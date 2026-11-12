module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.SetEventSpec where

import qualified Control.Exception as E
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (begin, close, commit, connectPostgreSQL)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Shared.Service.KnowledgeModel.Editor.EditorMapper (toKnowledgeModelEditorEvent)
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper (withEventIdentity)
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService (squashEventsForEditor)
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.Common
import Specs.Api.Handler.KnowledgeModelEditor.Event.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

setEventSpec requestContext =
  describe "setEvent" $ do
    test200 requestContext
    test200_resend requestContext
    test400 requestContext
    test400_otherEditor requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 requestContext = do
  create_test_200 "WS 200 OK" requestContext [] knowledgeModelEditorWebsocketEvent1'
  create_test_200
    "WS 200 OK if a required text is empty"
    requestContext
    []
    (toWebsocketEvent (editChapter1 e_km1_ch1__content {title = ChangedValue "", questionUuids = NothingChanged}))
  create_test_200
    "WS 200 OK if a reference becomes a cross reference without a target"
    requestContext
    amsterdamKnowledgeModelEditorEvents
    (toWebsocketEvent (e_km1_ch1_q2_rCh2_type {content = EditReferenceEvent' (EditCrossReferenceEvent' (EditCrossReferenceEvent NothingChanged NothingChanged NothingChanged))}))
  create_test_200_locked requestContext

create_test_200_locked requestContext =
  it "WS 200 OK if a batch holds the editor, validated against the batch" $
    -- GIVEN: Prepare request
    do
      let reqDto = toWebsocketEvent (a_km1_ch1_q1 {uuid = newEntityUuid, parentUuid = a_km1_ch4.entityUuid, entityUuid = newEntityUuid})
      -- AND: Prepare database
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- AND: Lock the editor by a batch that adds a chapter
      batchConnection <- lockEditorInBatch requestContext editor.uuid a_km1_ch4
      -- WHEN:
      write_SetReply c1 reqDto
      -- THEN:
      (nothingWasReceived c1 >> commit batchConnection) `E.onException` close batchConnection
      read_SetReply c1 reqDto
      read_SetReply c2 reqDto
      -- AND: Close sockets
      close batchConnection
      closeSockets [s1, s2]

create_test_200 title requestContext editorEvents reqDto =
  it title $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      runInContextIO (traverse_ insertKnowledgeModelEvent editorEvents) requestContext
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      ((c3, s3), (c4, s4)) <- connectTestWebsocketUsers requestContext leidenKnowledgeModelEditor.uuid
      -- WHEN:
      write_SetReply c1 reqDto
      -- THEN:
      read_SetReply c1 reqDto
      read_SetReply c2 reqDto
      nothingWasReceived c3
      nothingWasReceived c4
      -- AND: Find result in DB and compare with expectation state
      (Right editorFromDb) <- runInContext (findKnowledgeModelEditorByUuid editor.uuid) requestContext
      editorFromDb.squashed `shouldBe` False
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200_resend requestContext = do
  it "WS 200 OK if an event is sent again, echoed to the sender only and stored once" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- AND: Send the event
      write_SetReply c1 knowledgeModelEditorWebsocketEvent1'
      read_SetReply c1 knowledgeModelEditorWebsocketEvent1'
      read_SetReply c2 knowledgeModelEditorWebsocketEvent1'
      -- WHEN:
      write_SetReply c1 knowledgeModelEditorWebsocketEvent1'
      -- THEN:
      read_SetReply c1 knowledgeModelEditorWebsocketEvent1'
      nothingWasReceived c2
      -- AND: The event is stored once
      (Right events) <- runInContext (findKnowledgeModelEventsByEditorUuid editor.uuid) requestContext
      fmap (.uuid) events `shouldBe` [a_km1_ch4.uuid]
      -- AND: Close sockets
      closeSockets [s1, s2]
  it "WS 200 OK if an edit is sent again after a squash, both edits stay" $
    -- GIVEN: Prepare request
    do
      now <- getCurrentTimeInMicroseconds
      firstUuid <- generateUuid
      secondUuid <- generateUuid
      let editTitle title = editChapter1 e_km1_ch1__content {title = ChangedValue title, questionUuids = NothingChanged}
      let first = toWebsocketEvent (withEventIdentity firstUuid now (editTitle "First"))
      let second = toWebsocketEvent (withEventIdentity secondUuid (addUTCTime 0.001 now) (editTitle "Second"))
      -- AND: Prepare database
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- AND: Send both edits and squash
      write_SetReply c1 first
      traverse_ (`read_SetReply` first) [c1, c2]
      write_SetReply c1 second
      traverse_ (`read_SetReply` second) [c1, c2]
      runInContextIO (squashEventsForEditor editor.uuid) requestContext
      -- WHEN:
      write_SetReply c1 first
      -- THEN:
      read_SetReply c1 first
      nothingWasReceived c2
      -- AND: Both edits are stored once
      (Right events) <- runInContext (findKnowledgeModelEventsByEditorUuid editor.uuid) requestContext
      fmap (.uuid) events `shouldBe` [firstUuid, secondUuid]
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test400 requestContext =
  it "WS 400 BAD REQUEST if the event is structurally invalid, naming the rejected event" $
    -- GIVEN: Prepare request
    do
      let reqDto = toWebsocketEvent (a_km1_ch2_q4 {parentUuid = nonExistingUuid})
      -- AND: Prepare expectation
      let expError =
            ValidationError
              [_ERROR_VALIDATION__KM_EVENT_REJECTED (U.toString a_km1_ch2_q4.uuid)]
              (M.singleton "parentUuid" [_ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString nonExistingUuid) "chapter, list question or answer"])
      -- AND: Prepare database
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- WHEN:
      write_SetReply c1 reqDto
      -- THEN:
      read_EventError c1 expError knowledgeModelEditorWebsocketEvent1.uuid
      nothingWasReceived c2
      -- AND: Compare state in DB with expectation
      (Right events) <- runInContext (findKnowledgeModelEventsByEditorUuid editor.uuid) requestContext
      length events `shouldBe` 0
      -- AND: Close sockets
      closeSockets [s1, s2]

test400_otherEditor requestContext =
  it "WS 400 BAD REQUEST if the event uuid belongs to another editor" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      runInContextIO (insertKnowledgeModelEvent (toKnowledgeModelEditorEvent leidenKnowledgeModelEditor.uuid defaultTenantUuid a_km1_ch4)) requestContext
      -- AND: Prepare expectation
      let expError = UserError _ERROR_DATABASE__UNIQUE_CONSTRAINT_VIOLATION
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- WHEN:
      write_SetReply c1 knowledgeModelEditorWebsocketEvent1'
      -- THEN:
      read_EventError c1 expError knowledgeModelEditorWebsocketEvent1.uuid
      nothingWasReceived c2
      -- AND: Compare state in DB with expectation
      (Right events) <- runInContext (findKnowledgeModelEventsByEditorUuid editor.uuid) requestContext
      length events `shouldBe` 0
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
lockEditorInBatch requestContext editorUuid event = do
  connection <- connectPostgreSQL (BS.pack requestContext.serverConfig.database.connectionString)
  begin connection
  let batchContext = requestContext {dbConnection = Just connection} :: RequestContext
  (Right _) <- runInContext (findKnowledgeModelEditorByUuidLocked editorUuid) batchContext
  (Right _) <- runInContext (insertKnowledgeModelEvent (toKnowledgeModelEditorEvent editorUuid defaultTenantUuid event)) batchContext
  return connection

toWebsocketEvent :: KnowledgeModelEvent -> KnowledgeModelEditorWebSocketEventDTO
toWebsocketEvent event = AddKnowledgeModelEditorWebSocketEventDTO' (knowledgeModelEditorWebsocketEvent1 {event = event})

write_SetReply connection replyDto = do
  let reqDto = SetContent_ClientKnowledgeModelEditorMessageDTO replyDto
  sendMessage connection reqDto

read_SetReply connection expReplyDto = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerKnowledgeModelEditorMessageDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
