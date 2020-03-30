{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasql.Pool.Types where

import Hasql.Pool.Prelude
import Data.UUID (UUID)
import qualified Hasql.Connection
import qualified Data.Pool as ResourcePool
import Hasql.Session (QueryError(..), CommandError(ClientError))
import Data.ByteString (ByteString)

-- |
-- A pool of connections to DB.
data Pool
  = Pool
  { pool :: ResourcePool.Pool ConnectionWithId
  , settings :: Settings
  } deriving (Show)

-- |
-- A connection with an id for logging purposes
data ConnectionWithId = ConnectionWithId
  { connectionId :: ! UUID
  , connection :: ! Hasql.Connection.Connection
  }

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
  , loggingFn :: ByteString -> IO ()
  -- | ^ Used for logging purposes
  }

instance Show Settings where
  show (Settings { poolSize, timeout, connectionSettings }) = "Settings { poolSize = " <> show poolSize <> ", timeout = " <> show timeout <> ", connectionSettings = " <> show connectionSettings <> " }"

