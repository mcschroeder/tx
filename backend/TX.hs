{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TX
    ( Persistable(..)
    
    , Database(userData)
    , openDatabase
    , withUserData

    , TX
    , persistently
    , record
    , getData
    , liftSTM
    , throwTX

    , (<?>)
    , whenJust
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Maybe
import System.IO
import System.Directory

------------------------------------------------------------------------------

class (Show Update, Read Update) => Persistable d where
    data Update
    replay :: Update -> TX d ()

------------------------------------------------------------------------------

data Database d = Database { userData :: d
                           , logQueue :: TQueue Update
                           , logPath  :: FilePath
                           , _record  :: TQueue Update -> Update -> STM ()
                           }

openDatabase :: Persistable d => FilePath -> d -> IO (Database d)
openDatabase logPath userData = do
    initDbFile logPath
    logQueue <- newTQueueIO
    let db = Database { _record = const $ const $ return (), .. }
    readUpdates logPath >>= replayUpdates db
    forkIO $ serializer db
    return $ db { _record = writeTQueue }
  where initDbFile path = do 
            exists <- doesFileExist path
            unless exists $ withFile path WriteMode (\_ -> return ())

readUpdates :: Read Update => FilePath -> IO [Update]
readUpdates fp = map read . tail . lines <$> readFile fp

replayUpdates :: Persistable d => Database d -> [Update] -> IO ()
replayUpdates db = mapM_ (persistently db . replay)

withUserData :: Database d -> (d -> IO a) -> IO a
withUserData db act = act (userData db)

------------------------------------------------------------------------------

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
    Database {..} <- TX ask
    liftSTM $ _record logQueue u
{-# INLINE record #-}

getData :: TX d d
getData = userData <$> TX ask
{-# INLINE getData #-}

liftSTM :: STM a -> TX d a
liftSTM = TX . lift
{-# INLINE liftSTM #-}

throwTX :: Exception e => e -> TX d a
throwTX = liftSTM . throwSTM
{-# INLINE throwTX #-}

(<?>) :: Exception e => TX d (Maybe a) -> e -> TX d a
act <?> err = act >>= \case
    Nothing -> throwTX err
    Just x  -> return x

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x act = when (isJust x) (act $ fromJust x)
