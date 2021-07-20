module Evoli.Assignment.Externals.Application where

import Evoli.Assignment.Adapters.Servant
import Evoli.Assignment.Adapters.Sqlite
import Evoli.Assignment.Adapters.Sqlite.Config as Sqlite
import Evoli.Assignment.Config as Evoli
import Evoli.Assignment.UseCase

import Data.Aeson
import Polysemy.Error
import Polysemy.Input
import Polysemy.Log
import Servant
import Polysemy
import Database.SQLite.Simple hiding (Error)
import Evoli.Assignment.Model

createApp :: Evoli.Config -> Application
createApp cfg = serve assignmentAPI (liftServer cfg)

liftServer :: Evoli.Config -> ServerT AssignmentAPI Handler
liftServer cfg = hoistServer assignmentAPI interpretServer assignmentServer
  where
    interpretServer :: Sem '[InsuranceStor Quote, InsuranceStor Policy, Error UseCaseError, Log, Input Connection, Input Sqlite.Config, Embed IO] a -> Handler a
    interpretServer s = s
      & runBeamInsuranceStorage quotes
      & runBeamInsuranceStorage policies
      & runError @UseCaseError
      & interpretLogStderr'
      & runSqliteConnection
      & runInputConst (cfgStorage cfg)
      & runM
      & liftToHandler
      
    liftToHandler :: ToJSON e => IO (Either e a) -> Handler a
    liftToHandler = Handler . ExceptT . fmap handleErrors
    handleErrors (Left e)  = Left (err412 { errBody = encode (toJSON e) })
    handleErrors (Right v) = Right v
