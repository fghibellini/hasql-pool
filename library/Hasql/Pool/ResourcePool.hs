{-|
Extras for the resource-pool library.
-}
module Hasql.Pool.ResourcePool
where

import Hasql.Pool.Prelude
import Data.Pool


withResourceOnEither :: Pool resource -> (qfailure -> Bool) -> (resource -> IO (Either qfailure success)) -> IO (Either qfailure success)
withResourceOnEither pool check act = mask_ $ do
  (resource, localPool) <- takeResource pool
  failureOrSuccess <- act resource
  case failureOrSuccess of
    r@(Right success) -> do
      putResource localPool resource
      return r
    r@(Left failure) | check failure -> do
      putResource localPool resource
      return r
    r@(Left failure) | otherwise -> do
      destroyResource pool localPool resource
      return r
