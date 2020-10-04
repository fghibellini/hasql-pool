module Pool
(
  Pool,
  Settings(..),
  new,
  destroy,
  UsageError(..),
  use,
)
where

import Hasql.Pool.Prelude
import Hasql.Connection (Connection)
import Hasql.Session (Session)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session


-- |
-- A pool of connections to DB.
data Pool a =
  Pool
    Int
    {-^ Max connections. -}
    Int
    {-^ Connection timeout in milliseconds. -}
    Connection.Settings
    {-^ Connection settings. -}
    (TQueue (Resource a))
    {-^ Queue of active connections. -}
    (TVar Int)
    {-^ Queue size. -}
    (TVar Bool)
    {-^ Flag signaling whether pool's alive. -}

data Resource a =
  Resource {
    {-| Timestamp of last use. -}
    resourceLastTimestamp :: Int,
    resourceValue :: a
  }

-- TODO this is not used anywhere and it's not exported
loopCollectingGarbage :: Int -> TQueue a -> TVar Int -> TVar Bool -> IO ()
loopCollectingGarbage timeout queue queueSizeVar aliveVar =
  decide
  where
    decide =
      do
        ts <- getMillisecondsSinceEpoch
        join $ atomically $ do
          alive <- readTVar aliveVar
          if alive
            then do
              queueSize <- readTVar queueSizeVar
              if queueSize == 0
                then return (sleep (ts + timeout))
                else let
                  collect !queueSize !list =
                    do
                      entry@(Resource entryTs connection) <- readTQueue queue
                      if entryTs < ts
                        then collect (pred queueSize) (connection : list)
                        else do
                          writeTQueue queue entry
                          return (entryTs, queueSize, list)
                  in do
                    (ts, newQueueSize, list) <- collect queueSize []
                    writeTVar queueSizeVar newQueueSize
                    return (release list *> sleep ts *> decide)
            else do
              list <- flushTQueue queue
              writeTVar queueSizeVar 0
              return (release (fmap resourceValue list))
    sleep untilTs =
      do
        ts <- getMillisecondsSinceEpoch
        let
          diff =
            untilTs - ts
          in if diff > 0
            then threadDelay (diff * 1000)
            else return ()
    release =
      traverse_ Connection.release

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
type Settings =
  (Int, Int, Connection.Settings)

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
new :: Settings -> IO Pool
new (size, timeout, connectionSettings) =
  do
    queue <- newTQueueIO
    queueSizeVar <- newTVarIO 0
    aliveVar <- newTVarIO (size > 0)
    return (Pool size timeout connectionSettings queue queueSizeVar aliveVar)

-- |
-- Release the connection-pool.
destroy :: Pool -> IO ()
destroy (Pool _ _ _ _ _ aliveVar) =
  atomically (writeTVar aliveVar False)

-- |
-- A union over the connection establishment error and the session error.
data UsageError =
  ConnectionUsageError Connection.ConnectionError |
  SessionUsageError Session.QueryError |
  PoolIsReleasedUsageError
  deriving (Show, Eq)

-- |
-- Use a connection from the pool to run a session and
-- return the connection to the pool, when finished.
use :: Pool c -> Session.Session a -> IO (Either UsageError a)
use (Pool _ _ connectionSettings queue queueSizeVar aliveVar) session =
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
                return (useConnectionThenPutItToQueue connection)
          else return acquireConnectionThenUseThenPutItToQueue
      else return (return (Left PoolIsReleasedUsageError))
  where
    acquireConnectionThenUseThenPutItToQueue =
      do
        res <- Connection.acquire connectionSettings
        case res of
          Left acquisitionError -> return (Left (ConnectionUsageError acquisitionError))
          Right connection -> useConnectionThenPutItToQueue connection
    useConnectionThenPutItToQueue connection =
      do
        res <- Session.run session connection
        case res of
          Left queryError -> do
            Connection.release connection
            return (Left (SessionUsageError queryError))
          Right res -> do
            ts <- getMillisecondsSinceEpoch
            atomically $ do
              writeTQueue queue (Resource ts connection)
              modifyTVar' queueSizeVar succ
            return (Right res)

