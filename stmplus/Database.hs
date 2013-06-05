
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
    , what :: IORef [String]
    --, events :: ConcurrentIntMap (TVar Event)
    }

--lookup :: Int -> ConcurrentIntMap a -> IO a
--insert :: Int -> a -> ConcurrentIntMap a -> IO ()

--updateEvent :: EventId -> Aeson.Object -> Database MyData Event
--updateEvent eid delta = do
--    MyData{..} <- getData    
--    case unsafePerformIO $ lookup eid events of
--        Nothing -> throwSTM (EventNotFound eid)
--        Just eventTVar -> do
--            event <- liftSTM $ readTVar eventTVar
--            let event' = mergeJSON delta event
--            liftSTM $ writeTVar eventTVar event'
--            return event'




--main = do
    
--    replayFromDisk

--    transact db $ do
--        MyDB{..} <- getData
--        writeTVar 


--addEvent :: Event -> Database MyData ()
--addEvent event = do
--    MyData{..} <- getData
--    eventTVar <- liftSTM $ newTVar event
--    unsafePerformIO $ insert (eventId event) events



emptyData :: IO MyData
emptyData = do
    messagesTVar <- newTVarIO []
    usersTVar <- newTVarIO IntMap.empty
    eventsTVar <- newTVarIO IntMap.empty
    whatRef <- newIORef []
    return (MyData messagesTVar usersTVar eventsTVar whatRef)

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

data MyAction = PushMessage String
              | PopMessage
              | CreateUser

instance Recordable MyAction where
    replay (PushMessage x) = atomically $ pushMessage x
    replay (PopMessage) = atomically $ popMessage >> return ()
    replay CreateUser = createUser

createUser :: IORef (IntMap User) -> STMPlus UserId
createUser users = do
    uid <- unsafeIOToSTM randomIO
    let user = User uid Nothing []
    onCommit $ atomicModifyIORef users (IntMap.insert uid user)
    return user


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
