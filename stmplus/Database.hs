{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
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
    logQueue <- newTQueueIO
    let db = Database {..}
    forkIO $ serializer db
    return db

readUpdates :: Read Update => FilePath -> IO [Update]
readUpdates fp = map read . tail . lines <$> readFile fp

replayUpdates :: Persistable d => [Update] -> d -> IO ()
replayUpdates us userData = do
    logQueue <- newTQueueIO
    let db = Database { logPath = "", .. }
    sequence_ $ map (persistently db . replay) us

serializer :: Persistable d => Database d -> IO ()
serializer Database {..} = forever $ do
    u <- atomically $ readTQueue logQueue
    appendFile logPath ('\n':show u)

------------------------------------------------------------------------------

newtype TX d a = TX (ReaderT (Database d) STM a)
    deriving (Functor, Applicative, Monad)

persistently :: Database d -> TX d a -> IO a
persistently db (TX action) = atomically $ runReaderT action db

record :: Update -> TX d ()
record u = do
    Database {..} <- TX $ ask
    liftSTM $ writeTQueue logQueue u
{-# INLINE record #-}

getData :: TX d d
getData = userData <$> TX ask
{-# INLINE getData #-}

liftSTM :: STM a -> TX d a
liftSTM = TX . lift
{-# INLINE liftSTM #-}
