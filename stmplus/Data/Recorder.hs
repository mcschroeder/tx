module Data.Recorder where

import Control.Applicative
import Control.Concurrent.STMPlus
import System.IO

class Read a => Recordable a where
    replay :: a -> IO ()

class Monad m => Recorder m where
    record :: Recordable a => a -> m ()


appendToLog :: Recordable a => a -> IO ()
appendToLog = undefined

--replayFromDisk :: IO ()
--replayFromDisk = do
--    actions <- readActions
--    return ()
--    --sequence_ $ map replay actions

readActions :: Recordable a => IO [a]
readActions = map read . tail . lines <$> readFile "db.log"


instance Recorder STMPlus where
    record = onCommit . appendToLog

instance Recorder IO where
    record = appendToLog
