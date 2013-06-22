{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Concurrent.STMPlus
    ( STMPlus
    , atomically'
    , atomically
    , onCommit
    , liftSTM
    , getEnv
    ) where

import Control.Applicative
import Control.Concurrent.STM hiding (atomically)
import qualified Control.Concurrent.STM as STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

newtype STMPlus env a = STMPlus (ReaderT (CommitAction, env) STM a)
    deriving (Functor, Applicative, Monad)

type CommitAction = TVar (IO () -> IO ())

atomically' :: STMPlus () a -> IO a
atomically' = atomically ()

atomically :: env -> STMPlus env a -> IO a
atomically env (STMPlus action) = do
    commitActionTVar <- newTVarIO id
    join . STM.atomically . flip runReaderT (commitActionTVar, env) $ do
        result <- action
        commitAction <- lift $ readTVar commitActionTVar
        return (commitAction (return ()) >> return result)

onCommit :: IO () -> STMPlus env ()
onCommit io = do
    commitActionTVar <- STMPlus $ asks fst
    liftSTM $ do
        commitAction <- readTVar commitActionTVar
        writeTVar commitActionTVar (commitAction . (io >>))

-- TODO: for convenience, we probably want to get rid of this
--       and instead wrap all the STM primitives
--       (although that could have performance implications?)
liftSTM :: STM a -> STMPlus env a
liftSTM = STMPlus . lift
{-# INLINE liftSTM #-}

getEnv :: STMPlus env env
getEnv = STMPlus $ asks snd
{-# INLINE getEnv #-}
