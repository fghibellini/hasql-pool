{-# LANGUAGE OverloadedStrings #-}
{-|
Extras for the resource-pool library.
-}
module Hasql.Pool.ResourcePool
where

import Hasql.Pool.Prelude
import Hasql.Pool.Types
import Data.Pool hiding (Pool)
import Data.UUID (toASCIIBytes)
import Data.ByteString.Char8 (pack)


withResourceOnEither :: Show qfailure => Pool -> (qfailure -> Bool) -> (ConnectionWithId -> IO (Either qfailure success)) -> IO (Either qfailure success)
withResourceOnEither (Pool pool stgs) check act = mask_ $ do
  (resource, localPool) <- takeResource pool
  failureOrSuccess <- act resource
  case failureOrSuccess of
    r@(Right success) -> do
      putResource localPool resource
      return r
    r@(Left failure) | check failure -> do
      log $ "Query error on connection " <> toASCIIBytes (connectionId resource) <> ". Keeping connection. Query Error: \"" <> pack (show failure) <> "\""
      putResource localPool resource
      return r
    r@(Left failure) | otherwise -> do
      log $ "Query error on connection " <> toASCIIBytes (connectionId resource) <> ". Evicting connection. Query Error: \"" <> pack (show failure) <> "\""
      destroyResource pool localPool resource
      return r
  where
    log = loggingFn stgs
