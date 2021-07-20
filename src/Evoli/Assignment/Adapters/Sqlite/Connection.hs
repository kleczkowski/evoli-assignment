module Evoli.Assignment.Adapters.Sqlite.Connection where

import Evoli.Assignment.Adapters.Sqlite.Config

import Database.SQLite.Simple
import Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input

runSqliteConnection
  :: ( Embed IO `Member` r
     , Input Config `Member` r
     )
  => Sem (Input Connection ': r) a
  -> Sem r a
runSqliteConnection = Input.runInputSem $ do
  cfg <- Input.input
  embed $ if storageInMemory cfg
    then open ":memory:"
    else open (storageFileName cfg)

-- | Initialize sqlite database.
initializeTables
  :: ( Embed IO `Member` r
     , Input Connection `Member` r
     )
  => Sem r ()
initializeTables = do
  conn <- Input.input
  embed . execute_ conn $ "CREATE TABLE IF NOT EXISTS quotes   (applicant VARCHAR PRIMARY KEY, start_date INTEGER NOT NULL, end_date INTEGER NOT NULL, insured_item_price INTEGER NOT NULL);"
  embed . execute_ conn $ "CREATE TABLE IF NOT EXISTS policies (applicant VARCHAR PRIMARY KEY, start_date INTEGER NOT NULL, end_date INTEGER NOT NULL, insured_item_price INTEGER NOT NULL);"
