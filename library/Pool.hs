{-# LANGUAGE LambdaCase #-}
module Pool
(
  Pool,
  Settings(..),
  new,
  destroy,
  UsageError(..),
  withResource,
)
where

import Hasql.Pool.Prelude
import Hasql.Connection (Connection)
import Hasql.Session (Session)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Data.Bifunctor (first)


-- |
-- A pool of connections to DB.
data Pool e a =
  Pool
    Int
    {-^ Max connections. -}
    Int
    {-^ Connection timeout in milliseconds. -}
    (TQueue (Resource a))
    {-^ Queue of active connections. -}
    (TVar Int)
    {-^ Queue size. -}
    (TVar Bool)
    {-^ Flag signaling whether pool's alive. -}
    (IO (Either e a))
    {-^ Acquisition function. -}
    (a -> IO ())
    {-^ Release function. -}

data Resource a =
  Resource {
    {-| Timestamp of last use. -}
    resourceLastTimestamp :: Int,
    resourceValue :: a
  }

-- TODO this is not used anywhere and it's not exported
--loopCollectingGarbage :: Int -> TQueue a -> TVar Int -> TVar Bool -> IO ()
--loopCollectingGarbage timeout queue queueSizeVar aliveVar =
--  decide
--  where
--    decide =
--      do
--        ts <- getMillisecondsSinceEpoch
--        join $ atomically $ do
--          alive <- readTVar aliveVar
--          if alive
--            then do
--              queueSize <- readTVar queueSizeVar
--              if queueSize == 0
--                then return (sleep (ts + timeout))
--                else let
--                  collect !queueSize !list =
--                    do
--                      entry@(Resource entryTs connection) <- readTQueue queue
--                      if entryTs < ts
--                        then collect (pred queueSize) (connection : list)
--                        else do
--                          writeTQueue queue entry
--                          return (entryTs, queueSize, list)
--                  in do
--                    (ts, newQueueSize, list) <- collect queueSize []
--                    writeTVar queueSizeVar newQueueSize
--                    return (release list *> sleep ts *> decide)
--            else do
--              list <- flushTQueue queue
--              writeTVar queueSizeVar 0
--              return (release (fmap resourceValue list))
--    sleep untilTs =
--      do
--        ts <- getMillisecondsSinceEpoch
--        let
--          diff =
--            untilTs - ts
--          in if diff > 0
--            then threadDelay (diff * 1000)
--            else return ()
--    release =
--      traverse_ Connection.release

-- |
-- Settings of the connection pool. Consist of:
-- 
-- * Pool-size.
-- 
-- * Timeout.   
-- An amount of time in milliseconds for which the unused connections are kept open.
-- 
-- * Connection settings.
-- 
type Settings e a =
  (Int, Int, IO (Either e a), a -> IO ())

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
new :: Settings e c -> IO (Pool e c)
new (size, timeout, allocationFn, deallocationFn) =
  do
    queue <- newTQueueIO
    queueSizeVar <- newTVarIO 0
    aliveVar <- newTVarIO (size > 0)
    return (Pool size timeout queue queueSizeVar aliveVar allocationFn deallocationFn)

-- |
-- Release the connection-pool.
destroy :: (Pool e c) -> IO ()
destroy (Pool _ _ _ _ aliveVar _ _) =
  atomically (writeTVar aliveVar False)

data TakeError e =
  ConnectionUsageError e |
  PoolIsReleasedUsageError

takeResource :: Pool e c -> IO (Either (TakeError e) c)
takeResource (Pool _ _ queue queueSizeVar aliveVar allocate _) = do
  join $ atomically $ do
    alive <- readTVar aliveVar
    if alive
      then do
        queueSize <- readTVar queueSizeVar
        if queueSize > 0
          then do
            Resource _ connection <- readTQueue queue
            let
              !newQueueSize =
                succ queueSize -- TODO shouldn't this be predecessor ?
              in do
                writeTVar queueSizeVar newQueueSize
                return (return (Right connection))
          else return (first ConnectionUsageError <$> allocate)
      else return (return (Left PoolIsReleasedUsageError))

putResource :: Pool e c -> c -> IO ()
putResource (Pool _ _ queue queueSizeVar aliveVar _ _) res = do
  ts <- getMillisecondsSinceEpoch
  atomically $ do
    writeTQueue queue (Resource ts res)
    modifyTVar' queueSizeVar succ
  return ()

destroyResource :: Pool e c -> c -> IO ()
destroyResource (Pool _ _ queue queueSizeVar aliveVar _ deallocate) = deallocate

data UsageError e1 e2 =
  UsageTakeError (TakeError e1) |
  SessionUsageError e2

-- |
-- Use a connection from the pool to run a session and
-- return the connection to the pool, when finished.
withResource :: Pool e1 c -> (c -> IO (Either e2 a)) -> IO (Either (UsageError e1 e2) a)
withResource pool@(Pool _ _ queue queueSizeVar aliveVar _ _) operation = do
  takeResult <- takeResource pool
  case takeResult of
    Right resource -> useConnectionThenPutItToQueue resource
    Left e -> pure (Left (UsageTakeError e))
  where
    useConnectionThenPutItToQueue connection =
      do
        res <- operation connection
        case res of
          Left queryError -> do
            destroyResource pool connection
            return (Left (SessionUsageError queryError))
          Right res -> do
            putResource pool connection
            return (Right res)

