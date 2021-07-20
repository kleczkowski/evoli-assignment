module Spec (allTests) where

import qualified Evoli.Assignment.UseCaseSpec as UseCase
import qualified Evoli.Assignment.Adapters.SqliteSpec as Sqlite
import qualified Evoli.Assignment.Adapters.ServantSpec as Servant

import Test.Tasty

allTests :: TestTree
allTests = testGroup "All tests" 
  [ UseCase.spec
  , Sqlite.spec
  , Servant.spec
  ]