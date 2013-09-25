{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TX
    ( Persistable(..)

      -- * Managing the database
    , Database(userData)
    , openDatabase
    , closeDatabase
    , withUserData

      -- * The TX monad
    , TX
    , persistently
    , record
    , getData
    , liftSTM
    , throwTX
    , unsafeIOToTX

      -- * Utility functions
    , (<?>)
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
import GHC.Conc
import System.IO

------------------------------------------------------------------------------

-- | The type family at the heart of TX.
--
-- You make any data type you want to use with the TX monad an instance of
-- 'Persistable' and define 'Update' constructors for each of the methods
-- acting on this type that you want to be able to record during a transaction.
-- Then you implement 'replay' in such a way that for each of the Update
-- constructors, the appropiate method is called.
--
-- Example:
--
-- > data MyDB = MyDB { posts :: TVar [String] }
-- >
-- > instance Persistable MyDB where
-- >     data Update = CreatePost String
-- >                 | ModifyPost Int String
-- >
-- >     replay (CreatePost p)   = void $ createPost p
-- >     replay (ModifyPost n p) = modifyPost n p
--
-- where @createPost@ and @modifyPost@ are functions in the TX monad:
--
-- > createPost :: String -> TX MyDB Int
-- > createPost p = do
-- >     record (CreatePost p)
-- >     (MyDB posts) <- getData
-- >     liftSTM $ do
-- >         ps <- readTVar posts
-- >         writeTVar posts (ps ++ [p])
-- >         return $ length ps
-- >
-- > modifyPost :: Int -> String -> TX MyDB ()
-- > modifyPost n p = do
-- >     record (ModifyPost n p)
-- >     (MyDB posts) <- getData
-- >     liftSTM $ do
-- >         ps <- readTVar posts
-- >         let (xs,ys) = splitAt n ps
-- >             ps'     = xs ++ p : (tail ys)
-- >         writeTVar posts ps'
--
-- Note that @Update@ also needs to be an instance of 'SafeCopy'. Currently,
-- it's not possible to derive SafeCopy instances for associated types
-- automatically, so you have to do it by hand:
--
-- > instance SafeCopy Update where
-- >     putCopy (CreatePost p)   = contain $ putWord8 0 >> safePut p
-- >     putCopy (ModifyPost n p) = contain $ putWord8 1 >> safePut n >> safePut p
-- >     getCopy = contain $ do
-- >         getWord8 >>= \case
-- >             0 -> CreatePost <$> safeGet
-- >             1 -> ModifyPost <$> safeGet <*> safeGet
class SafeCopy Update => Persistable d where
    data Update
    replay :: Update -> TX d ()

-- TODO: provide automatic derivation using TH

------------------------------------------------------------------------------

-- | An opaque type wrapping any kind of user data for use in the 'TX' monad.
data Database d = Database { userData :: d
                           , logHandle :: Handle
                           , logQueue :: TQueue Update
                           , _record :: TQueue Update -> Update -> STM ()
                           , serializerTid :: MVar ThreadId
                           }

-- | Opens the database at the given path or creates a new one.
openDatabase :: Persistable d
             => FilePath  -- ^ Location of the log file.
             -> d  -- ^ Base data. Any existing log is replayed on top of this.
             -> IO (Database d)
openDatabase logPath userData = do
    putStr ("Opening " ++ logPath ++ " database... ")
    logHandle <- openBinaryFile logPath ReadWriteMode
    logQueue <- newTQueueIO
    serializerTid <- newEmptyMVar
    let _db = Database { _record = const $ const $ return (), .. }
    hIsEOF logHandle >>= flip unless (replayUpdates _db)
    putMVar serializerTid =<< forkIO (serializer _db)
    let db = _db { _record = writeTQueue }
    putStrLn ("DONE")
    return db

-- TODO: throw actual error when operating on a closed database
-- | Close a database. Blocks until all pending recordings been serialized.
-- Using a database after it has been closed is an error.
closeDatabase :: Database d -> IO ()
closeDatabase Database {..} = do
    atomically $ check =<< isEmptyTQueue logQueue
    killThread =<< takeMVar serializerTid
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

-- | A thin wrapper around STM. The main feature is the ability to 'record'
-- updates of the underlying data.
newtype TX d a = TX (ReaderT (Database d) STM a)
    deriving (Functor, Applicative, Monad)

-- | Perform a series of TX actions persistently.
--
-- Note that there is no guarantee that all recorded updates have been serialized
-- when the functions returns. As such, durability is only partially guaranteed.
--
-- Since this calls 'atomically' on the underlying STM actions,
-- the same caveats apply (e.g. you can't use it inside 'unsafePerformIO').
persistently :: Database d -> TX d a -> IO a
persistently db (TX action) = atomically $ runReaderT action db

-- | Record an 'Update' to be serialized to disk when the transaction commits.
-- If the transaction retries, the update is still only recorded once.
-- If the transaction aborts, the update is not recorded at all.
record :: Update -> TX d ()
record u = do
    Database {..} <- TX ask
    liftSTM $ _record logQueue u
{-# INLINE record #-}

-- | Get the user data from the database.
getData :: TX d d
getData = userData <$> TX ask
{-# INLINE getData #-}

-- | Run STM actions inside TX.
liftSTM :: STM a -> TX d a
liftSTM = TX . lift
{-# INLINE liftSTM #-}

-- | Throw an exception in TX, which will abort the transaction.
--
-- @throwTX = liftSTM . throwSTM@
throwTX :: Exception e => e -> TX d a
throwTX = liftSTM . throwSTM
{-# INLINE throwTX #-}

-- | Unsafely performs IO in the TX monad. Highly dangerous!
-- The same caveats as with 'unsafeIOToSTM' apply.
--
-- @unsafeIOToTX = liftSTM . unsafeIOToSTM@
unsafeIOToTX :: IO a -> TX d a
unsafeIOToTX = liftSTM . unsafeIOToSTM
{-# INLINE unsafeIOToTX #-}

------------------------------------------------------------------------------

-- | @act \<?\> err = maybe (throwTX err) return =<< act@
(<?>) :: Exception e => TX d (Maybe a) -> e -> TX d a
act <?> err = maybe (throwTX err) return =<< act
