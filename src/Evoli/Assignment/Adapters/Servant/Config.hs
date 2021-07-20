module Evoli.Assignment.Adapters.Servant.Config where

import Dhall
import Dhall.Deriving

newtype Config = Config
  { servantPort :: Int
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)
