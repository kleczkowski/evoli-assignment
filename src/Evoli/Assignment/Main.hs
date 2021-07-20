module Evoli.Assignment.Main where

import Dhall
import Evoli.Assignment.Externals.Application
import Evoli.Assignment.Adapters.Servant.Config
import Evoli.Assignment.Config
import Network.Wai.Handler.Warp (run)

-- | Entry point of the service
main :: IO ()
main = do
  config <- input auto "./config.dhall"
  let p = servantPort . cfgServant $ config
  putStrLn $ "Starting server on port " <> show p
  run (fromIntegral p) (createApp config)