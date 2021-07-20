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
    then open ":inmemory:"
    else open (storageFileName cfg)

-- | Initialize sqlite database.
initializeTables
  :: ( Embed IO `Member` r
     , Input Connection `Member` r
     )
  => Sem r ()
initializeTables = do
  conn <- Input.input
  embed . execute_ conn $ "CREATE TABLE quotes   (applicant VARCHAR NOT NULL, start_date INTEGER NOT NULL, end_date INTEGER NOT NULL, insured_item_price INTEGER NOT NULL, PRIMARY_KEY(applicant));"
  embed . execute_ conn $ "CREATE TABLE policies (applicant VARCHAR NOT NULL, start_date INTEGER NOT NULL, end_date INTEGER NOT NULL, insured_item_price INTEGER NOT NULL, PRIMARY_KEY(applicant));"
