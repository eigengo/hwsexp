{-# LANGUAGE OverloadedStrings #-}
module Timed(timed) where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Time.Clock
import Control.Exception (fromException, handle)
import Control.Monad (forM_, forever)
import Control.Monad.Writer (liftIO)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, threadDelay)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

timed :: IO ()
timed = do
  putStrLn "Server is running"
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn  <- WS.acceptRequest pending
  query <- WS.receiveData conn
  clients <- liftIO $ readMVar state
  let client = (query, conn)
  liftIO $ modifyMVar_ state $ return . addClient client
  tick conn state client

tick :: WS.Connection -> MVar ServerState -> Client -> IO ()
tick conn state client@(user, _) = handle catchDisconnect $
  forever $ do
    time <- getCurrentTime
    WS.sendTextData conn (T.append user (T.pack $ show time))
    putStrLn $ "Tick..." ++ show time
    threadDelay 1000000
  where
    catchDisconnect e = case fromException e of
      Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ return . removeClient client
      _ -> return ()
