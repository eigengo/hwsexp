{-# LANGUAGE OverloadedStrings #-}
module Main(main) where
import Data.Text (Text)
import Control.Applicative ((<$>))
import Control.Exception (fromException, handle, SomeException)
import Control.Monad (forM_, forever, replicateM)
import Control.Monad.Writer (liftIO)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, threadDelay)
import System.Random (randomIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

-- |Client is a combination of the statement that we're running and the
--  WS connection that we can send results to
type Client = (Text, WS.Connection)

-- |Server state is simply an array of active @Client@s
type ServerState = [Client]

-- |Named function that retuns an empty @ServerState@
newServerState :: ServerState
newServerState = []

-- |Adds new client to the server state
addClient :: Client      -- ^ The client to be added
          -> ServerState -- ^ The current state
          -> ServerState -- ^ The state with the client added
addClient client clients = client : clients

-- |Removes an existing client from the server state
removeClient :: Client      -- ^ The client being removed
             -> ServerState -- ^ The current state (hopefully with the client included)
             -> ServerState -- ^ The state with the client removed
removeClient client = filter ((/= fst client) . fst)

-- |The handler for the application's work
application :: MVar ServerState -- ^ The server state
            -> WS.ServerApp     -- ^ The server app that will handle the work
application state pending = do
  conn  <- WS.acceptRequest pending
  query <- WS.receiveData conn
  clients <- liftIO $ readMVar state
  let client = (query, conn)
  modifyMVar_ state (\s' -> return (addClient client s'))
  perform state client


-- |Performs the query on behalf of the client, cleaning up after itself when the client disconnects
perform :: MVar ServerState -- ^ The server state
        -> Client           -- ^ The client tuple (the query to perform and the connection for the responses)
        -> IO ()            -- ^ The output
perform state client@(query, conn) = handle catchDisconnect $
  forever $ do
    numbers <- replicateM 100 ((`mod` 100) <$> randomIO :: IO Int)
    WS.sendTextData conn (T.pack $ show numbers)
    threadDelay 1000000
  where
    catchDisconnect :: SomeException -> IO ()
    catchDisconnect _ = liftIO $ modifyMVar_ state $ return . removeClient client
    {--
    catchDisconnect e = case fromException e of
      Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ return . removeClient client
      _ -> return ()
    --}

-- |The main entry point for the WS application
main :: IO ()
main = do
  putStrLn "Server is running"
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9160 $ application state
