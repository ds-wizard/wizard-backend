module Specs.Api.Handler.Project.Detail_WS.DeleteItemSpec where

import Data.Aeson
import qualified Data.Map.Strict as M
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.Migration.Development.Project.Data.ProjectEventBatches
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Service.Project.Event.ProjectEventMapper

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Project.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

deleteItemSpec requestContext =
  describe "deleteItem" $ do
    test200 requestContext
    test400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 requestContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
    do
      insertProjectAndUsers requestContext project10
      runInContextIO (insertProjectEvents project10Events) requestContext
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      -- WHEN:
      write_DeleteItem c1 (toEventChangeDTO (dite_rQ4_it1' project10Uuid))
      -- THEN:
      read_DeleteItem c1 (toEventDTO (dite_rQ4_it1' project10Uuid) (Just userAlbert))
      read_DeleteItem c2 (toEventDTO (dite_rQ4_it1' project10Uuid) (Just userAlbert))
      read_DeleteItem c3 (toEventDTO (dite_rQ4_it1' project10Uuid) (Just userAlbert))
      -- AND: The item and its replies are deleted
      assertProjectReplyValuesInDB requestContext project10.uuid (fmap (.value) fRepliesWithDeletedItem)
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test400 requestContext =
  it "WS 400 BAD REQUEST - the item doesn't exist" $
    -- GIVEN: Prepare database
    do
      insertProjectAndUsers requestContext project10
      runInContextIO (insertProjectEvents project10Events) requestContext
      -- AND: Prepare request
      let reqDto = diche_rQ4_it1 {itemUuid = rQ4_it3} :: DeleteItemEventChangeDTO
      -- AND: Prepare expectation
      let expError = ValidationError [] (M.singleton "itemUuid" [_ERROR_VALIDATION__PROJECT_EVENT_ITEM_ABSENCE rQ4_it3])
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      -- WHEN:
      write_DeleteItem c1 (DeleteItemEventChangeDTO' reqDto)
      -- THEN:
      read_EventError c1 expError reqDto.uuid
      nothingWasReceived c2
      nothingWasReceived c3
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_DeleteItem connection eventDto = do
  let reqDto = SetContent_ClientProjectMessageDTO eventDto
  sendMessage connection reqDto

read_DeleteItem connection expEventDto = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerProjectMessageDTO eventDto))) = eResult
  expEventDto `shouldBe` eventDto
