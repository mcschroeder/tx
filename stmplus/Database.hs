
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database where

import Database.Monad

import Control.Concurrent.STM

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-------------------------------------------------------------------------------

data MyData = MyData
    { dbMessages :: TVar [String]
    , dbUsers    :: TVar (IntMap (TVar User))
    , dbEvents   :: TVar (IntMap (TVar Event))
    }

emptyData :: IO MyData
emptyData = do
    messagesTVar <- newTVarIO []
    usersTVar <- newTVarIO IntMap.empty
    eventsTVar <- newTVarIO IntMap.empty
    return (MyData messagesTVar usersTVar eventsTVar)

type UserId = Int
type EventId = Int

data User = User
    { userId :: Int
    , userName :: String
    , userEvents :: [EventId]
    }

data Event = Event
    { eventId :: Int
    , eventTitle :: String
    }

-------------------------------------------------------------------------------

data MyEvent = PushMessage String
             | PopMessage
             deriving (Eq, Show, Read)

instance RecordableEvent MyData MyEvent where
    replay (PushMessage x) = pushMessage x
    replay PopMessage      = popMessage >> return ()

pushMessage :: String -> Database MyData ()
pushMessage x = do
    MyData{..} <- getData
    liftSTM $ modifyTVar' dbMessages (x:)
    record (PushMessage x)

popMessage :: Database MyData (Maybe String)
popMessage = do
    MyData{..} <- getData
    msgs <- liftSTM $ readTVar dbMessages
    case msgs of
        (x:xs) -> do
            liftSTM $ writeTVar dbMessages xs
            record PopMessage
            return (Just x)
        _ -> do
            return Nothing

peekMessage :: MyData -> STM (Maybe String)
peekMessage db = do
    msgs <- readTVar (dbMessages db)
    case msgs of
        (x:_) -> return $ Just x
        []    -> return Nothing

allMessages :: MyData -> IO [String]
allMessages db = readTVarIO (dbMessages db)
