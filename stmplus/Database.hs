{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

newtype TX d a = TX (ReaderT (Env d) STM a)
    deriving (Functor, Applicative, Monad)

data Env d = Env { userData :: d, 
                   queue :: (Maybe (TQueue String)) }

persistently :: Update u d => d -> TX d () -> IO ()
persistently d (TX action) = do
    q <- newTQueueIO
    let env = Env d (Just q)
    atomically $ flip runReaderT env action

transiently :: Update u d => d -> TX d () -> IO ()
transiently d (TX action) = do
    let env = Env d Nothing
    atomically $ flip runReaderT env action

replayAll :: Update u d => [u] -> d -> IO ()
replayAll us d = sequence_ $ map (transiently d . replay) us

class (Show u, Read u) => Update u d | d -> u where
    replay :: u -> TX d ()

liftSTM :: STM a -> TX d a
liftSTM = TX . lift
{-# INLINE liftSTM #-}

getData :: TX d d
getData = do
    Env d _ <- TX $ ask
    return d
{-# INLINE getData #-}

record :: Update u d => u -> TX d ()
record u = do
    Env _ q <- TX $ ask
    case q of
        Just queue -> liftSTM $ writeTQueue queue (show u)
        Nothing -> return ()
