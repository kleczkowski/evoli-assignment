module Evoli.Assignment.Adapters.Sqlite.Config where

import Dhall
import Dhall.Deriving

data Config = Config
  { storageFileName :: !FilePath
  , storageInMemory :: !Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)