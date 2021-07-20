{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Evoli.Assignment.Adapters.Sqlite.Model where

import Data.Time
import Database.Beam
import GHC.TypeLits

type Insurance tag = InsuranceT tag Identity
type InsuranceId tag = PrimaryKey (InsuranceT tag) Identity
type Quote = "quote"
type Policy = "policy"

data InsuranceT tag f = Insurance
  { _insuranceApplicant        :: Columnar f Text
  , _insuranceStartDate        :: Columnar f Day
  , _insuranceEndDate          :: Columnar f Day
  , _insuranceInsuredItemPrice :: Columnar f Int64
  } deriving stock    (Generic)
    deriving anyclass (Beamable)
deriving instance Show (Insurance tag)
deriving instance Eq (Insurance tag)

instance KnownSymbol tag => Table (InsuranceT tag) where
  data PrimaryKey (InsuranceT tag) f =
    InsuranceKey (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = InsuranceKey . _insuranceApplicant

data InsuranceDb f = InsuranceDb
  { _insuranceQuotes   :: f (TableEntity (InsuranceT Quote))
  , _insurancePolicies :: f (TableEntity (InsuranceT Policy))
  } deriving stock (Generic)
    deriving anyclass (Database be)

insuranceDb :: DatabaseSettings be InsuranceDb
insuranceDb = defaultDbSettings
