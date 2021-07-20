{-# LANGUAGE RecordWildCards #-}
module Evoli.Assignment.Model.Types where

import Data.Aeson
import Data.Time.Calendar
import Data.Scientific
import Servant (FromHttpApiData)

-- | An unique applicant name.
newtype Applicant = Applicant
  { applicantName :: Text
  } deriving stock      (Eq, Show, Ord)
    deriving newtype    (FromJSON, ToJSON, FromHttpApiData)

-- | Barbie, tagged data type representing an insurance quote or policy.
data Insurance tag = Insurance
  { insuranceStartDate :: !Day
  , insuranceEndDate   :: !Day
  , insuredItemPrice   :: !Scientific
  } deriving stock      (Eq, Show, Generic)
    deriving anyclass   (FromJSON, ToJSON)

-- | Phantom type indicating insurance quote.
type Quote = "quote"

-- | Phantom type indicating insurance policy.
type Policy = "policy"

-- | An insurance price.
newtype InsurancePrice = InsurancePrice
  { insurancePrice :: Scientific
  } deriving stock        (Eq, Show, Generic)
    deriving anyclass     (FromJSON, ToJSON)

-- | Computes an insurance price according to provided quote.
computeInsurancePrice :: Insurance Quote -> InsurancePrice
computeInsurancePrice = InsurancePrice . (* 0.2) . insuredItemPrice

-- | Converts an insurance quote to insurance policy. Structure remains untouched.
toInsurancePolicy :: Insurance Quote -> Insurance Policy
toInsurancePolicy Insurance{..} = Insurance{..}
