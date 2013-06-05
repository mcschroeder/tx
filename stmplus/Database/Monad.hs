{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Database.Monad 
    ( Database
    , transact
    , onCommit
    , liftSTM
    , getData
    
    , RecordableEvent(..)
    , record
    , transactFromRecord
    ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-------------------------------------------------------------------------------

newtype Database db a = Database (ReaderT (TransactionEnv db) STM a)
    deriving (Functor, Applicative, Monad)

data TransactionEnv db = TransactionEnv
    { commitAction :: TVar (IO () -> IO ())
    , database :: db
    }


transact :: db -> Database db a -> IO a
transact db (Database action) = do
    commitActionTVar <- newTVarIO id
    let env = TransactionEnv commitActionTVar db
    join . atomically . flip runReaderT env $ do
        result <- action
        commitAction <- lift $ readTVar commitActionTVar
        return (commitAction (return ()) >> return result)

onCommit :: IO () -> Database db ()
onCommit io = do
    commitActionTVar <- Database $ asks commitAction
    liftSTM $ do
        commitAction <- readTVar commitActionTVar
        writeTVar commitActionTVar (commitAction . (io >>))

liftSTM :: STM a -> Database db a
liftSTM = Database . lift
{-# INLINE liftSTM #-}

getData :: Database db db
getData = Database $ asks database >>= return
{-# INLINE getData #-}

-------------------------------------------------------------------------------

class (Show ev, Read ev) => RecordableEvent db ev | db -> ev, ev -> db where
    replay :: ev -> Database db ()

record :: RecordableEvent db ev => ev -> Database db ()
record = onCommit . appendFile "db.log" . (:) '\n' . show
{-# INLINE record #-}

transactFromRecord :: RecordableEvent db ev => db -> IO ()
transactFromRecord db = do
    events <- persistedEvents
    replayEvents events db

persistedEvents :: RecordableEvent db ev => IO [ev]
persistedEvents = do
    xs <- readFile "db.log"
    return $ map read (tail $ lines xs)

replayEvents :: RecordableEvent db ev => [ev] -> db -> IO ()
replayEvents events db = do
    let (Database action) = foldl (\act ev -> act . ((replay ev) >>)) id events $ return ()
    dummy <- newTVarIO id
    let env = TransactionEnv dummy db
    atomically $ runReaderT action env
