module Evoli.Assignment.Adapters.Sqlite.Handler
  ( runBeamInsuranceStorage
  , quotes
  , policies
  ) where

import qualified Evoli.Assignment.Adapters.Sqlite.Model as Beam
import Evoli.Assignment.Interfaces.Storage
import qualified Evoli.Assignment.Model as Domain
import qualified Evoli.Assignment.UseCase as UC

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import GHC.TypeLits
import Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Evoli.Assignment.Adapters.Sqlite.Connection

-- | Runs insurance storage using Beam.
runBeamInsuranceStorage
  :: forall tag r a .
     ( Embed IO `Member` r
     , Input Connection `Member` r
     , KnownSymbol tag
     )
  => (forall f. Beam.InsuranceDb f -> f (TableEntity (Beam.InsuranceT tag))) -- ^ Table selector from @InsuranceDb@.
  -> Sem (UC.InsuranceStor tag ': r) a
  -> Sem r a
runBeamInsuranceStorage tblSel s = initializeTables >> runBeamInsuranceStorage' tblSel s

runBeamInsuranceStorage'
  :: forall tag r a .
     ( Embed IO `Member` r
     , Input Connection `Member` r
     , KnownSymbol tag
     )
  => (forall f. Beam.InsuranceDb f -> f (TableEntity (Beam.InsuranceT tag)))
  -> Sem (UC.InsuranceStor tag ': r) a
  -> Sem r a
runBeamInsuranceStorage' tblSel = interpret $ \case
  StorageGet _ k    -> beamGet k tblSel
  StoragePut k v    -> beamPut k v tblSel
  StorageDelete _ k -> beamDelete k tblSel

-- | Selects quotes table.
quotes :: Beam.InsuranceDb f -> f (TableEntity (Beam.InsuranceT Beam.Quote))
quotes = Beam._insuranceQuotes

-- | Selects policies table.
policies :: Beam.InsuranceDb f -> f (TableEntity (Beam.InsuranceT Beam.Policy))
policies = Beam._insurancePolicies

toDomainInsurance :: Beam.Insurance tag -> Domain.Insurance tag
toDomainInsurance bi = Domain.Insurance
  { Domain.insuranceStartDate = Beam._insuranceStartDate bi
  , Domain.insuranceEndDate = Beam._insuranceEndDate bi
  , Domain.insuredItemPrice = fromIntegral (Beam._insuranceInsuredItemPrice bi) / 100
  }

toBeamInsurance :: Domain.Applicant -> Domain.Insurance tag -> Beam.InsuranceT tag Identity
toBeamInsurance app di = Beam.Insurance
  { Beam._insuranceApplicant = Domain.applicantName app
  , Beam._insuranceStartDate = Domain.insuranceStartDate di
  , Beam._insuranceEndDate = Domain.insuranceEndDate di
  , Beam._insuranceInsuredItemPrice = floor (Domain.insuredItemPrice di * 100)
  }

beamGet
  :: forall tag r .
     ( Embed IO `Member` r
     , Input Connection `Member` r
     , KnownSymbol tag
     )
  => Domain.Applicant
  -> (forall f. Beam.InsuranceDb f -> f (TableEntity (Beam.InsuranceT tag)))
  -> Sem r (Maybe (Domain.Insurance tag))
beamGet k tblSel = do
  conn <- Input.input
  beamInsurance <- embed
    . runBeamSqlite conn
    . runSelectReturningOne
    . select $ do
        ins <- all_ (tblSel Beam.insuranceDb)
        guard_ (Beam._insuranceApplicant ins ==. val_ (Domain.applicantName k))
        pure ins
  pure (toDomainInsurance <$> beamInsurance)

beamPut
  :: forall tag r .
     ( Embed IO `Member` r
     , Input Connection `Member` r
     , KnownSymbol tag
     )
  => Domain.Applicant
  -> Domain.Insurance tag
  -> (forall f. Beam.InsuranceDb f -> f (TableEntity (Beam.InsuranceT tag)))
  -> Sem r ()
beamPut k v tblSel  = do
  conn <- Input.input
  embed
    . runBeamSqlite conn
    . runInsert
    . insert (tblSel Beam.insuranceDb)
    $ insertValues [toBeamInsurance k v]

beamDelete
  :: forall tag r .
     ( Embed IO `Member` r
     , Input Connection `Member` r
     , KnownSymbol tag
     )
  => Domain.Applicant
  -> (forall f. Beam.InsuranceDb f -> f (TableEntity (Beam.InsuranceT tag)))
  -> Sem r ()
beamDelete k tblSel = do
  conn <- Input.input
  embed
    . runBeamSqlite conn
    . runDelete
    $ delete (tblSel Beam.insuranceDb)
             (\ins -> Beam._insuranceApplicant ins ==. val_ (Domain.applicantName k))

