--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Database where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import System.IO

newtype TX d a = TX (ReaderT (Env d) STM a)
    deriving (Functor, Applicative, Monad)


class (Show Update, Read Update) => Persistent d where
    data Update :: *
    replay :: Update -> TX d ()

data Env d where
    Env :: Persistent d => TQueue Update -> d -> Env d

loadEnv :: Persistent d => d -> IO (Env d)
loadEnv d = do
    q <- newTQueueIO
    forkIO $ watcher q   -- TODO: ensure only one watcher exists at any one time
    return (Env q d)

watcher :: Show Update => TQueue Update -> IO ()
watcher q = forever $ do
    u <- atomically $ readTQueue q
    appendFile "db.log" ('\n':show u)

readUpdates :: Read Update => IO [Update]
readUpdates = map read . tail . lines <$> readFile "db.log"

persistently :: Env d -> TX d () -> IO ()
persistently env (TX action) = do
    atomically $ flip runReaderT env action

replayAll :: [Update] -> Env d -> IO ()
replayAll us (Env _ d) = do
    q' <- newTQueueIO
    let env = Env q' d
    sequence_ $ map (persistently env . replay) us


--class (Show u, Read u) => Update u d where
--    replay :: u -> TX u d ()

liftSTM :: STM a -> TX d a
liftSTM = TX . lift
{-# INLINE liftSTM #-}

getData :: TX d d
getData = do
    Env _ d <- TX $ ask
    return d
{-# INLINE getData #-}

record :: Update -> TX d ()
record u = do
    Env q _ <- TX $ ask
    liftSTM $ writeTQueue q u
