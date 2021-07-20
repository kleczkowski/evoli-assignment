module Evoli.Assignment.Adapters.ServantSpec where

import Evoli.Assignment.Interfaces.Storage
import Evoli.Assignment.Model
import Evoli.Assignment.UseCase

import qualified Data.Map.Strict as M
import Data.Time.Calendar
import Polysemy
import Polysemy.Error
import Polysemy.Log
import Polysemy.State
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree 
spec = testGroup "Servant adapter spec" 
  [
  ]

