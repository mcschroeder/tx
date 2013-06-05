{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Concurrent.STMPlus
    ( STMPlus
    , persistently
    , atomically
    , onCommit
    , liftSTM

    , Update(..)
    , replayUpdates
    , record
    ) where

import Control.Applicative
import Control.Concurrent.STM hiding (atomically)
import qualified Control.Concurrent.STM as STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

newtype STMPlus u a = STMPlus (ReaderT (Env u) STM a)
    deriving (Functor, Applicative, Monad)

data Env u = Env { commitAction :: TVar (IO () -> IO ())
                 , updateQueue :: TQueue u}

atomically :: Update u db => STMPlus u a -> IO a
atomically action = do
    -- TODO: hack
    dummyQueue <- newTQueueIO
    persistently dummyQueue action

persistently :: Update u db => TQueue u -> STMPlus u a -> IO a
persistently updateQueue (STMPlus action) = do
    commitActionTVar <- newTVarIO id
    let env = Env commitActionTVar updateQueue
    join . STM.atomically . flip runReaderT env $ do
        result <- action
        commitAction <- lift $ readTVar commitActionTVar
        return (commitAction (return ()) >> return result)

onCommit :: Update u db => IO () -> STMPlus u ()
onCommit io = do
    commitActionTVar <- STMPlus $ asks commitAction
    liftSTM $ do
        commitAction <- readTVar commitActionTVar
        writeTVar commitActionTVar (commitAction . (io >>))

-- TODO: for convenience, we probably want to get rid of this
--       and instead wrap all the STM primitives
--       (although that could have performance implications?)
liftSTM :: Update u db => STM a -> STMPlus u a
liftSTM = STMPlus . lift
{-# INLINE liftSTM #-}

------------------------------------------------------------------------------

class (Show u, Read u) => Update u db | u -> db, db -> u where
    replay :: u -> db -> IO ()

replayUpdates :: Update u db => db -> [u] -> IO ()
replayUpdates = mapM_ . flip replay

readUpdates :: Update u db => IO [u]
readUpdates = map read . tail . lines <$> readFile "db.log"

record :: Update u db => u -> STMPlus u ()
record update = do
    updateQueue <- STMPlus $ asks updateQueue
    liftSTM $ writeTQueue updateQueue update
