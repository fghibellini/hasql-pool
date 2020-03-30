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
import Hasql.Pool.Types
import qualified Hasql.Connection
import qualified Hasql.Session
import Hasql.Session (QueryError(..), CommandError(ClientError))
import qualified Data.Pool as ResourcePool
import qualified Hasql.Pool.ResourcePool as ResourcePool
import Control.Monad.Error.Class (liftEither)
import Data.Either.Combinators (mapLeft)
import Control.Exception (throwIO)
import System.Random (randomIO)
import Data.UUID (toASCIIBytes)

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
  , loggingFn = \_ -> pure ()
  }

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire stgs@(Settings { poolSize, timeout, connectionSettings, loggingFn }) =
  Pool
    <$> ResourcePool.createPool acquire release stripes timeout poolSize
    <*> pure stgs
  where
    acquire =
      join (either throwIO pure <$> do
        res <- Hasql.Connection.acquire connectionSettings
        case res of
          Left e -> pure $ Left e
          Right c -> do
            rid <- randomIO
            loggingFn $ "Allocated connection " <> toASCIIBytes rid
            pure $ Right $ ConnectionWithId rid c
        )
    release x = do
      Hasql.Connection.release $ connection x
      loggingFn $ "Released connection " <> toASCIIBytes (connectionId x)
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
use pool@(Pool { settings }) session =
  ResourcePool.withResourceOnEither pool (connectionHealthCheck settings) (Hasql.Session.run session . connection)
