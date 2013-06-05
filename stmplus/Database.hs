{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Database where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

newtype TX env a = TX (ReaderT env STM a)
    deriving (Functor, Applicative, Monad)

data Env u d where
    Env :: Update u d => TQueue u -> d -> Env u d

loadEnv :: Update u d => d -> IO (Env u d)
loadEnv d = do
    q <- newTQueueIO
    -- forkIO $ watcher q   -- TODO: ensure only one watcher exists at any one time
    return (Env q d)

watcher :: Update u d => TQueue u -> IO ()
watcher q = forever $ do
    u <- atomically $ readTQueue q
    print u  -- TODO

persistently :: Env u d -> TX (Env u d) () -> IO ()
persistently env (TX action) = do
    atomically $ flip runReaderT env action

replayAll :: Update u d => [u] -> (Env u d) -> IO ()
replayAll us (Env _ d) = do
    q' <- newTQueueIO
    let env = Env q' d
    sequence_ $ map (persistently env . replay) us

class (Show u, Read u) => Update u d | d -> u where
    replay :: u -> TX (Env u d) ()

liftSTM :: STM a -> TX d a
liftSTM = TX . lift
{-# INLINE liftSTM #-}

getData :: TX (Env u d) d
getData = do
    Env _ d <- TX $ ask
    return d
{-# INLINE getData #-}

record :: Update u d => u -> TX (Env u d) ()
record u = do
    Env q _ <- TX $ ask
    liftSTM $ writeTQueue q u
