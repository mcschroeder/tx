--{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Recorder where

import Control.Concurrent.STMPlus
import Control.Concurrent.STM.TQueue

class (Show r, Read r) => Recordable r where
    replay :: r -> d -> IO ()

--record :: Recordable u db => u -> STMPlus u ()
--record update = do
--    queue <- getEnv
--    updateQueue <- STMPlus $ asks updateQueue
--    liftSTM $ writeTQueue updateQueue update


--replayUpdates :: Recordable u db => db -> [u] -> IO ()
--replayUpdates = mapM_ . flip replay

--readUpdates :: Recordable u db => IO [u]
--readUpdates = map read . tail . lines <$> readFile "db.log"


type RecSTM d a = STMPlus (RecEnv d) a

data RecEnv d = RecEnv { queue :: Recordable r => TQueue r
                       , userData :: d }

class Monad m => Recorder m where
    record :: Recordable r => r -> m ()

instance Recorder (STMPlus (RecEnv d)) where
    record r = do
        RecEnv q _ <- getEnv
        liftSTM $ writeTQueue q r


loadRecordings :: d -> IO (RecEnv d)
loadRecordings db = do
    q <- newTQueueIO
    return $ RecEnv q db


