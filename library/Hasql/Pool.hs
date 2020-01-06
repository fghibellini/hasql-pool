{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasql.Pool
(
  Pool,
  Settings(..),
  defaultSettings,
  acquire,
  release,
  use,
)
where

import Hasql.Pool.Prelude
import qualified Hasql.Connection
import qualified Hasql.Session
import Hasql.Session (QueryError(..), CommandError(ClientError))
import qualified Data.Pool as ResourcePool
import qualified Hasql.Pool.ResourcePool as ResourcePool
import Control.Monad.Error.Class (liftEither)
import Data.Either.Combinators (mapLeft)
import Control.Exception (throwIO)


-- |
-- A pool of connections to DB.
data Pool
  = Pool
  { pool :: ResourcePool.Pool Hasql.Connection.Connection
  , settings :: Settings
  } deriving (Show)

-- | Settings of the connection pool. Consist of:
data Settings
  = Settings
  { poolSize :: Int
  -- | ^ size of the pool
  , timeout :: NominalDiffTime
  -- | ^ An amount of time for which an unused resource is kept open. The smallest acceptable value is 0.5 seconds.
  , connectionSettings :: Hasql.Connection.Settings
  -- | ^ Connection settings.
  , connectionHealthCheck :: QueryError -> Bool
  -- | ^ Function called on an error returned by a @Session@ to evaluate whether the connection is still healthy.
  -- | When False is returned, the connection is evicted from the pool.
  }

instance Show Settings where
  show (Settings { poolSize, timeout, connectionSettings }) = "Settings { poolSize = " <> show poolSize <> ", timeout = " <> show timeout <> ", connectionSettings = " <> show connectionSettings <> " }"

defaultConnectionHealthCheck :: QueryError -> Bool
defaultConnectionHealthCheck (QueryError _ _ (ClientError (Just "no connection to the server"))) = False
defaultConnectionHealthCheck _ = True

defaultSettings :: Settings
defaultSettings
  = Settings
  { poolSize = 5
  , timeout = 60.0 :: NominalDiffTime
  , connectionSettings = ""
  , connectionHealthCheck = defaultConnectionHealthCheck
  }

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire stgs@(Settings { poolSize, timeout, connectionSettings }) =
  Pool
    <$> ResourcePool.createPool acquire release stripes timeout poolSize
    <*> pure stgs
  where
    acquire =
      join (either throwIO pure <$> Hasql.Connection.acquire connectionSettings)
    release =
      Hasql.Connection.release
    stripes =
      1

-- |
-- Release the connection-pool.
release :: Pool -> IO ()
release (Pool { pool }) =
  ResourcePool.destroyAllResources pool

-- |
-- Use a connection from the pool to run a session and
-- return the connection to the pool, when finished.
use :: Pool -> Hasql.Session.Session a -> IO (Either Hasql.Session.QueryError a)
use (Pool { pool, settings }) session =
  ResourcePool.withResourceOnEither pool (connectionHealthCheck settings) $
  Hasql.Session.run session
