{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Concurrent.STMPlus
    ( STMPlus
    , atomically
    , onCommit
    , liftSTM
    ) where

import Control.Applicative
import Control.Concurrent.STM hiding (atomically)
import qualified Control.Concurrent.STM as STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

newtype STMPlus a = STMPlus (ReaderT CommitAction STM a)
    deriving (Functor, Applicative, Monad)

type CommitAction = TVar (IO () -> IO ())

atomically :: STMPlus a -> IO a
atomically (STMPlus action) = do
    commitActionTVar <- newTVarIO id
    join . STM.atomically . flip runReaderT commitActionTVar $ do
        result <- action
        commitAction <- lift $ readTVar commitActionTVar
        return (commitAction (return ()) >> return result)

onCommit :: IO () -> STMPlus ()
onCommit io = do
    commitActionTVar <- STMPlus $ ask
    liftSTM $ do
        commitAction <- readTVar commitActionTVar
        writeTVar commitActionTVar (commitAction . (io >>))

-- TODO: for convenience, we probably want to get rid of this
--       and instead wrap all the STM primitives
--       (although that could have performance implications?)
liftSTM :: STM a -> STMPlus a
liftSTM = STMPlus . lift
{-# INLINE liftSTM #-}
