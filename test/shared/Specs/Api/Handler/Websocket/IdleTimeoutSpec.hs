module Specs.Api.Handler.Websocket.IdleTimeoutSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import qualified Control.Exception as E
import Control.Monad (replicateM_, void)
import Data.Aeson (Value)
import qualified Data.Text as T
import qualified Data.UUID as U
import Network.Socket
import Network.WebSockets (defaultConnectionOptions, makePendingConnection, sendTextData)
import Test.Hspec hiding (shouldBe)

import Shared.Api.Handler.Websocket
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Project.Project
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

idleTimeoutSpec :: RequestContext -> SpecWith ()
idleTimeoutSpec requestContext =
  describe "idle timeout" $ do
    test_idleClose requestContext
    test_pingKeepsOpen requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_idleClose :: RequestContext -> SpecWith ()
test_idleClose requestContext =
  it "WS 200 OK - a client that sends nothing within the idle timeout is closed with 1012 and leaves the user list" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      -- AND: Connect a watching client
      (c1, s1) <- createConnection requestContext (reqUrlT project.uuid (Just reqAuthToken))
      read_SetUserList c1 0
      withIdleTimeoutServer requestContext project.uuid $ \port -> do
        -- WHEN: Connect a client that sends nothing
        (c2, s2) <- createConnectionOnPort port "/"
        read_SetUserList c2 1
        read_SetUserList c1 1
        -- THEN:
        read_CloseCode c2 1012
        read_SetUserList c1 0
        -- AND: Close sockets
        closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_pingKeepsOpen :: RequestContext -> SpecWith ()
test_pingKeepsOpen requestContext =
  it "WS 200 OK - pings keep the connection open past the idle timeout" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      withIdleTimeoutServer requestContext project.uuid $ \port -> do
        (c1, s1) <- createConnectionOnPort port "/"
        read_SetUserList c1 0
        -- WHEN: Ping for longer than the idle timeout
        replicateM_ 3 $ do
          threadDelay (idleTimeoutMicros `div` 2)
          sendTextData c1 (T.pack "ping")
          read_Pong c1
        -- THEN: Closed only once the pings stop
        read_CloseCode c1 1012
        -- AND: Close sockets
        closeSockets [s1]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
idleTimeoutMicros :: Int
idleTimeoutMicros = 1000000

withIdleTimeoutServer :: RequestContext -> U.UUID -> (Int -> IO a) -> IO a
withIdleTimeoutServer requestContext projectUuid action =
  E.bracket listenOnLoopback close $ \listening -> do
    port <- fromIntegral <$> socketPort listening
    withAsync (serveOne listening) (const (action port))
  where
    serverContext = requestContext {dbConnection = Nothing} :: RequestContext
    serveOne listening = do
      (sock, _) <- accept listening
      pendingConnection <- makePendingConnection sock defaultConnectionOptions
      void . runInContext (runIdleWebsocket pendingConnection) $ serverContext
    runIdleWebsocket pendingConnection =
      runWebsocketWith
        (Just idleTimeoutMicros)
        pendingConnection
        (putUserOnline projectUuid)
        (deleteUser projectUuid)
        (\connectionUuid outbound -> handleRawData connectionUuid outbound (const Nothing) (\_ (_ :: Value) -> return ()))

listenOnLoopback :: IO Socket
listenOnLoopback = do
  sock <- socket AF_INET Stream 6
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1))) `E.onException` close sock
  listen sock 1
  return sock
