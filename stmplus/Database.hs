{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database 
    ( Persistable(..)
    
    , Database(userData)
    , openDatabase

    , TX
    , persistently
    , record
    , getData
    , liftSTM
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import System.IO

------------------------------------------------------------------------------

class (Show Update, Read Update) => Persistable d where
    data Update
    replay :: Update -> TX d ()

------------------------------------------------------------------------------

data Database d = Database { userData :: d
                           , logQueue :: TQueue Update
                           , logPath :: FilePath
                           }

openDatabase :: Persistable d => FilePath -> d -> IO (Database d)
openDatabase logPath userData = do
    us <- readUpdates logPath
    replayUpdates us userData
    q <- newTQueueIO
    let db = Database userData q logPath
    forkIO $ serializer db
    return db

readUpdates :: Read Update => FilePath -> IO [Update]
readUpdates fp = map read . tail . lines <$> readFile fp

replayUpdates :: Persistable d => [Update] -> d -> IO ()
replayUpdates us userData = do
    dummyQueue <- newTQueueIO
    let db = Database userData dummyQueue ""
    sequence_ $ map (persistently db . replay) us

serializer :: Persistable d => Database d -> IO ()
serializer (Database _ q logPath) = forever $ do
    u <- atomically $ readTQueue q
    appendFile logPath ('\n':show u)

------------------------------------------------------------------------------

newtype TX d a = TX (ReaderT (Database d) STM a)
    deriving (Functor, Applicative, Monad)

persistently :: Database d -> TX d () -> IO ()
persistently db (TX action) = atomically $ runReaderT action db

record :: Update -> TX d ()
record u = do
    Database _ q _ <- TX $ ask
    liftSTM $ writeTQueue q u
{-# INLINE record #-}

getData :: TX d d
getData = do
    Database d _ _ <- TX $ ask
    return d
{-# INLINE getData #-}

liftSTM :: STM a -> TX d a
liftSTM = TX . lift
{-# INLINE liftSTM #-}

