{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STMPlus
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM hiding (atomically)

-------------------------------------------------------------------------------

data MyData = MyData { messages :: TVar [String] }

emptyData :: IO MyData
emptyData = do
    messages <- newTVarIO []
    return $ MyData messages

-------------------------------------------------------------------------------

--pushMessage :: String -> MyData -> STMPlus MyDataUpdate ()
pushMessage :: String -> TX MyData ()
pushMessage x = do
    MyData{..} <- getEnv
    liftSTM $ modifyTVar' messages (x:)
    record (PushMessage x)

popMessage :: TX MyData (Maybe String)
popMessage = do
    MyData{..} <- getEnv
    msgs <- liftSTM $ readTVar messages
    case msgs of
        (x:xs) -> do
            liftSTM $ writeTVar messages xs
            record PopMessage
            return (Just x)
        _ -> do
            return Nothing


data MyDataUpdate = PushMessage String
                  | PopMessage
                  deriving (Show, Read)

instance Update MyDataUpdate where
    replay (PushMessage x) = undefined --atomically . pushMessage x
    replay PopMessage      = undefined --\db -> (atomically (popMessage db) >> return ())

-- example of a database method that simply does not record anything
peekMessage :: TX MyData (Maybe String)
peekMessage db = do
    msgs <- liftSTM $ readTVar (messages db)
    case msgs of
        (x:_) -> return $ Just x
        []    -> return Nothing

-- example of a method 'orthogonal' to the monad
allMessages :: MyData -> IO [String]
allMessages db = readTVarIO (messages db)

-------------------------------------------------------------------------------

main = do
    db <- emptyData

    updateQueue <- newTQueueIO
    forkIO $ watchQueue updateQueue

    persistently updateQueue $ do
        pushMessage "hello" db
        pushMessage "world" db


watchQueue :: Update u db => TQueue u -> IO ()
watchQueue q = forever $ do
    u <- STM.atomically $ readTQueue q
    print u

