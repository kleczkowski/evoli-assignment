module Evoli.Assignment.Config where

import Dhall
import Dhall.Deriving

import qualified Evoli.Assignment.Adapters.Servant.Config as Servant
import qualified Evoli.Assignment.Adapters.Sqlite.Config as Sqlite

data Config = Config
  { cfgStorage :: !Sqlite.Config
  , cfgServant :: !Servant.Config
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)
