module Main where

import Control.Concurrent.STM

import Database
import Database.Monad

main = do
    db <- emptyData
    
    transactFromRecord db

    --transact db $ do
    --    pushMessage "hello"
    --    pushMessage "world"

    --xs <- transact db $ do
    --    x <- popMessage
    --    y <- popMessage
    --    return $ reverse [x,y]

    --print xs


    --replayEvents [PushMessage "hello", PushMessage "what", PopMessage, PushMessage "world"] db
    --replayEvents [PopMessage] db

    msgs <- allMessages db
    print msgs