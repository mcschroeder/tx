{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Database

-------------------------------------------------------------------------------

data MyData = MyData { messages :: TVar [String] }

emptyData :: IO MyData
emptyData = do
    messages <- newTVarIO []
    return $ MyData messages

-------------------------------------------------------------------------------

pushMessage :: String -> TX MyData ()
pushMessage x = do
    MyData{..} <- getData
    liftSTM $ modifyTVar' messages (x:)
    record (PushMessage x)

popMessage :: TX MyData (Maybe String)
popMessage = do
    MyData{..} <- getData
    msgs <- liftSTM $ readTVar messages
    case msgs of
        (x:xs) -> do
            liftSTM $ writeTVar messages xs
            record PopMessage
            return (Just x)
        _ -> do
            return Nothing

instance Persistent MyData where
    data Update = PushMessage String
                | PopMessage
                deriving (Show, Read)
    replay (PushMessage x) = pushMessage x
    replay (PopMessage) = popMessage >> return ()


--data MyDataUpdate = PushMessage String
--                  | PopMessage
--                  deriving (Show, Read)

--instance Update MyDataUpdate MyData where
--    replay (PushMessage x) = pushMessage x
--    replay PopMessage      = popMessage >> return ()

-- example of a database method that simply does not record anything
peekMessage :: TX MyData (Maybe String)
peekMessage = do
    MyData{..} <- getData
    msgs <- liftSTM $ readTVar messages
    case msgs of
        (x:_) -> return $ Just x
        []    -> return Nothing

-- example of a method 'orthogonal' to the monad
allMessages :: MyData -> IO [String]
allMessages db = readTVarIO (messages db)

-------------------------------------------------------------------------------

main = do
    def <- emptyData
    env <- loadEnv def

    us <- readUpdates
    replayAll us env

    --persistently env $ do
    --    pushMessage "hello"
    --    pushMessage "world"

    let (Env _ d) = env
    msgs <- allMessages d

    print msgs

