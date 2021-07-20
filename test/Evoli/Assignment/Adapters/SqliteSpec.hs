module Evoli.Assignment.Adapters.SqliteSpec (spec) where

import Evoli.Assignment.Adapters.Sqlite.Config
import Evoli.Assignment.Adapters.Sqlite.Connection
import Evoli.Assignment.Adapters.Sqlite.Handler
import Evoli.Assignment.Interfaces.Storage (Storage)
import qualified Evoli.Assignment.Interfaces.Storage as Storage
import Evoli.Assignment.UseCase

import Database.SQLite.Simple
import Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input

import Data.Time
import Evoli.Assignment.Model
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec = testGroup "SQLite adapter test"
  [ testCase "should persist an insurance" $ runAllEffects $ do
      let applicant = Applicant "abc"
          quote = Insurance
              { insuranceStartDate = fromGregorian 2021 7 21
              , insuranceEndDate = fromGregorian 2021 7 30
              , insuredItemPrice = 10.75
              }
      Storage.storagePut applicant (quote :: Insurance Quote)
      quote' <- Storage.storageGet (Proxy :: Proxy (Insurance Quote)) applicant
      case quote' of
        Nothing -> embed (assertFailure "storage does not persist an insurance")
        Just q -> embed (assertBool "quotes should be equal" (quote == q))
  , testCase "should remove an insurance" $ runAllEffects $ do
      let applicant = Applicant "def"
          quote = Insurance
              { insuranceStartDate = fromGregorian 2021 7 21
              , insuranceEndDate = fromGregorian 2021 7 30
              , insuredItemPrice = 10.75
              }
      Storage.storagePut applicant (quote :: Insurance Quote)
      Storage.storageDelete (Proxy :: Proxy (Insurance Quote)) applicant
      quote' <- Storage.storageGet (Proxy :: Proxy (Insurance Quote)) applicant
      case quote' of
        Nothing -> pass
        Just _ -> embed (assertFailure "still an insurance persists")
  ]

runAllEffects :: Sem '[InsuranceStor Quote, Input Connection, Input Config, Embed IO] a -> IO a
runAllEffects s = (initializeTables >> s)
  & runBeamInsuranceStorage quotes
  & runSqliteConnection
  & Input.runInputConst (Config "test.db" False) -- in-memory db
  & runM
