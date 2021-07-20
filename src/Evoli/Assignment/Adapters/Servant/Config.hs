module Evoli.Assignment.Adapters.Servant.Config where

import Dhall
import Dhall.Deriving

newtype Config = Config
  { servantPort :: Natural
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)
