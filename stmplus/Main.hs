{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Typeable

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
        _ -> throw $ StackError "stack empty" 

instance Persistable MyData where
    data Update = PushMessage String
                | PopMessage
                deriving (Show, Read)
    replay (PushMessage x) = pushMessage x
    replay (PopMessage) = void popMessage 


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

data StackError = StackError String deriving (Typeable,Show) 

instance TXException StackError 
instance Exception StackError

main = do
    base <- emptyData
    db <- openDatabase "db2.log" base

    result <- persistently db $ do
                 pushMessage "hello"
                 pushMessage "world"
                 mapM (const popMessage) [1..1]

    case result of
        Left e -> putStrLn $ "Stackexn: " ++ show (e::StackError)
        Right a -> print a

    msgs <- allMessages base

    print msgs

