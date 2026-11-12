module Specs.Api.Handler.Project.Detail_WS.AddCommentSpec where

import Data.Aeson
import Data.Foldable (traverse_)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Error.Error
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Project
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

addCommentSpec requestContext =
  describe "addComment" $ do
    test200 requestContext
    test200_resend requestContext
    test400_otherProject requestContext
    test403_privateThread requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 requestContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
    do
      runInContext U.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO (insertProject project7) requestContext
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      ((c4, s4), (c5, s5), (c6, s6)) <- connectTestWebsocketUsers requestContext project7.uuid
      -- WHEN:
      write_AddComment c1 acche_rQ2_t1_1'
      -- THEN:
      read_AddComment c1 ace_rQ2_t1_1'
      read_AddComment c2 ace_rQ2_t1_1'
      read_AddComment c3 ace_rQ2_t1_1'
      nothingWasReceived c4
      nothingWasReceived c5
      nothingWasReceived c6
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200_resend requestContext =
  it "WS 200 OK - a comment sent again is echoed to the sender only and stored once" $
    -- GIVEN: Prepare database
    do
      insertProjectAndUsers requestContext project10
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      -- AND: Send the comment
      write_AddComment c1 acche_rQ2_t1_1'
      traverse_ (`read_AddComment` ace_rQ2_t1_1') [c1, c2, c3]
      -- WHEN:
      write_AddComment c1 acche_rQ2_t1_1'
      -- THEN:
      read_AddComment c1 ace_rQ2_t1_1'
      nothingWasReceived c2
      nothingWasReceived c3
      -- AND: The comment is stored once
      (Right (Just thread)) <- runInContext (findProjectCommentThreadById acche_rQ2_t1_1.threadUuid) requestContext
      fmap (.uuid) thread.comments `shouldBe` [acche_rQ2_t1_1.commentUuid]
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test400_otherProject requestContext =
  it "WS 400 BAD REQUEST - a comment uuid of another project is rejected" $
    -- GIVEN: Prepare database
    do
      insertProjectAndUsers requestContext project10
      runInContextIO (insertProject project7) requestContext
      -- AND: Store the comment in the other project
      ((c4, s4), (c5, s5), (c6, s6)) <- connectTestWebsocketUsers requestContext project7.uuid
      write_AddComment c4 acche_rQ2_t1_1'
      read_AddComment c4 ace_rQ2_t1_1'
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      -- AND: Prepare expectation
      let expError = UserError _ERROR_DATABASE__UNIQUE_CONSTRAINT_VIOLATION
      threadUuid <- generateUuid
      -- WHEN: Send the same comment in a new thread
      write_AddComment c1 (AddCommentEventChangeDTO' (acche_rQ2_t1_1 {threadUuid = threadUuid} :: AddCommentEventChangeDTO))
      -- THEN:
      read_EventError c1 expError acche_rQ2_t1_1.uuid
      nothingWasReceived c2
      nothingWasReceived c3
      nothingWasReceived c4
      -- AND: No thread is left behind
      (Right mThread) <- runInContext (findProjectCommentThreadById threadUuid) requestContext
      fmap (.uuid) mThread `shouldBe` Nothing
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test403_privateThread requestContext =
  it "WS 403 FORBIDDEN - a comment of a private thread sent again by a non-editor" $
    -- GIVEN: Prepare database
    do
      let project = project10 {visibility = PrivateProjectVisibility, sharing = AnyoneWithLinkCommentProjectSharing}
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      -- AND: Connect an editor and an anonymous commenter
      (c1, s1) <- createConnection requestContext (reqUrlT project.uuid (Just reqAuthToken))
      read_SetUserList c1 0
      (c2, s2) <- createConnection requestContext (reqUrlT project.uuid Nothing)
      read_SetUserList c1 1
      read_SetUserList c2 1
      -- AND: The editor adds a private comment
      write_AddComment c1 (AddCommentEventChangeDTO' (acche_rQ2_t1_1 {private = True} :: AddCommentEventChangeDTO))
      read_AddComment c1 ace_rQ2_t1_1'
      nothingWasReceived c2
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Project"
      -- WHEN: The commenter sends the same comment
      write_AddComment c2 acche_rQ2_t1_1'
      -- THEN:
      read_EventError c2 expError acche_rQ2_t1_1.uuid
      nothingWasReceived c1
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_AddComment connection replyDto = do
  let reqDto = SetContent_ClientProjectMessageDTO replyDto
  sendMessage connection reqDto

read_AddComment connection expReplyDto = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerProjectMessageDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
