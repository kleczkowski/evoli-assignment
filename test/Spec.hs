module Spec (allTests) where

import qualified Evoli.Assignment.UseCaseSpec as UseCase

import Test.Tasty

allTests :: TestTree
allTests = testGroup "All tests" 
  [ UseCase.spec
  ]