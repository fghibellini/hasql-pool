module Main where

import BasePrelude
import Test.Hspec
import Hasql.Pool
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Session (QueryError(..), QueryError(..), CommandError(..), ResultError(..))
import qualified Hasql.Statement as Statement
import Data.Either (isLeft)


main = hspec $ do
  describe "Hasql.Pool.use" $ do

    it "throws an error if a connection could not be establised" $ do
      pool <- acquire $ defaultSettings { connectionSettings = "host=localhost port=5432 user=postgres_INVALID dbname=postgres" }
      let
        statement = Statement.Statement "" mempty Decoders.noResult True
        session = Session.statement () statement
        in do
          use pool session `shouldThrow` (\(_ :: Connection.ConnectionError) -> True)

    -- TODO this test would make sense if we were able to inspect whether a second connection was created
    --it "releases a spot in the pool when there is an error" $ do
    --  pool <- acquire $ defaultSettings
    --    { connectionSettings = "host=localhost port=5432 user=postgres dbname=postgres"
    --    , poolSize = 1
    --    , connectionHealthCheck = const False
    --    }
    --  let
    --    statement = Statement.Statement "" mempty Decoders.noResult True
    --    session = Session.statement () statement
    --    in do
    --      use pool session `shouldReturn` (Left (QueryError "" [] (ResultError (UnexpectedResult "Unexpected result status: EmptyQuery"))))
    --  let
    --    session = let
    --      statement = let
    --        decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
    --        in Statement.Statement "SELECT 1" mempty decoder True
    --      in Session.statement () statement
    --    in do
    --      use pool session `shouldReturn` (Right 1)
