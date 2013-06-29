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
    , closeDatabase
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
import qualified Data.ByteString as B
import Data.Maybe
import Data.SafeCopy
import Data.Serialize
import System.IO

------------------------------------------------------------------------------

class SafeCopy Update => Persistable d where
    data Update
    replay :: Update -> TX d ()

------------------------------------------------------------------------------

data Database d = Database { userData  :: d
                           , logHandle :: Handle
                           , logQueue  :: TQueue Update
                           , _record   :: TQueue Update -> Update -> STM ()
                           }

openDatabase :: Persistable d => FilePath -> d -> IO (Database d)
openDatabase logPath userData = do
    putStr ("Opening " ++ logPath ++ " database... ")
    logHandle <- openBinaryFile logPath ReadWriteMode
    logQueue <- newTQueueIO
    let _db = Database { _record = const $ const $ return (), .. }
    replayUpdates _db
    forkIO $ serializer _db
    let db = _db { _record = writeTQueue }
    putStrLn ("DONE")
    return db

closeDatabase :: Database d -> IO ()
closeDatabase Database {..} =
    -- TODO: wait for the serializer to finish & kill its thread
    hClose logHandle

replayUpdates :: Persistable d => Database d -> IO ()
replayUpdates db = mapDecode (persistently db . replay)
                             (B.hGetSome (logHandle db) 1024)

-- | 'mapDecode' @f nextChunk@ repeatedly calls @nextChunk@ to get a
-- 'B.ByteString', (partially) decodes this string using 'safeGet' and
-- and then applies @f@ to the (final) result. This continues until
-- @nextChunk@ returns an empty ByteString.
mapDecode :: SafeCopy a => (a -> IO ()) -> IO B.ByteString -> IO ()
mapDecode f nextChunk = go run =<< nextChunk
    where
        run = runGetPartial safeGet
        go k c = case k c of
            Fail    err  -> error ("TX.mapDecode: " ++ err)
            Partial k'   -> go k' =<< nextChunk
            Done    u c' -> f u >> if B.null c'
                                       then do c'' <- nextChunk
                                               if B.null c''
                                                   then return ()
                                                   else go run c''
                                       else go run c'

withUserData :: Database d -> (d -> IO a) -> IO a
withUserData db act = act (userData db)

------------------------------------------------------------------------------

serializer :: Persistable d => Database d -> IO ()
serializer Database {..} = forever $ do
    u <- atomically $ readTQueue logQueue
    let str = runPut (safePut u)
    B.hPut logHandle str

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
