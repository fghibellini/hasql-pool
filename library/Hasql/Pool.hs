module Hasql.Pool
(
  Pool,
  Settings(..),
  acquire,
  release,
  UsageError(..),
  use,
)
where

import Hasql.Pool.Prelude
import Hasql.Connection (Connection)
import Hasql.Session (Session)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Pool as P
import Data.Bifunctor (first)

newtype Pool = Pool (P.Pool Connection.ConnectionError Connection.Connection)

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
acquire :: Settings -> IO Pool
acquire (size, timeout, connectionSettings) = Pool <$> P.new (size, timeout, Connection.acquire connectionSettings, Connection.release)

-- |
-- Release the connection-pool.
release :: Pool -> IO ()
release (Pool p) = P.destroy p

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
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use (Pool p) session = first mapErrors <$> P.withResource p (Session.run session)
  where
    mapErrors (P.UsageTakeError (P.ConnectionUsageError e)) = ConnectionUsageError e
    mapErrors (P.UsageTakeError P.PoolIsReleasedUsageError) = PoolIsReleasedUsageError
    mapErrors (P.SessionUsageError e) = SessionUsageError e

