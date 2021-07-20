module Evoli.Assignment.UseCase where

import Evoli.Assignment.Interfaces.Storage (Storage)
import qualified Evoli.Assignment.Interfaces.Storage as Storage
import Evoli.Assignment.Model

import Data.Aeson (FromJSON, ToJSON)
import Data.Validation
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log

-- | Type alias for insurance storages.
type InsuranceStor tag = Storage Applicant (Insurance tag)

-- | Data type describing an error during business logic.
data UseCaseError
  = QuoteValidationErrors ![QuoteValidationError]
  | InsuranceAlreadyExists !Applicant
  | InsuranceDoesNotExists !Applicant
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Generates new quote and stores it. Returns computed insurance price.
createQuote
  :: ( InsuranceStor Quote  `Member` r
     , Error UseCaseError   `Member` r
     , Log                  `Member` r
     )
  => Applicant
  -> Insurance Quote
  -> Sem r InsurancePrice
createQuote k insurance = do
  valid <- validation (Error.throw . QuoteValidationErrors) pure 
         . validateQuote 
         $ insurance
  whenJustM (Storage.storageGet (Proxy :: Proxy (Insurance Quote)) k) . const $
    Error.throw (InsuranceAlreadyExists k)
  Storage.storagePut k valid
  Log.info ("New quote has been created for applicant: " <> applicantName k)
  pure (computeInsurancePrice valid)

-- | Removes an insurance quote issued by given applicant.
discardQuote
  :: ( InsuranceStor Quote  `Member` r
     , Error UseCaseError   `Member` r
     , Log                  `Member` r
     )
  => Applicant
  -> Sem r ()
discardQuote k = do
  _ <- whenNothingM (Storage.storageGet (Proxy :: Proxy (Insurance Quote)) k) $ 
    Error.throw (InsuranceDoesNotExists k)
  Storage.storageDelete (Proxy :: Proxy (Insurance Quote)) k
  Log.info ("Deleted insurance quote from applicant: " <> applicantName k)

-- | Accepts a quote, transforming it to policy.
acceptQuote
  :: ( InsuranceStor Quote  `Member` r
     , InsuranceStor Policy `Member` r
     , Error UseCaseError   `Member` r
     , Log                  `Member` r
     )
  => Applicant
  -> Sem r ()
acceptQuote k = do
  quote <- whenNothingM (Storage.storageGet (Proxy :: Proxy (Insurance Quote)) k) $ 
    Error.throw (InsuranceDoesNotExists k)
  Storage.storagePut k (toInsurancePolicy quote)
  Log.info ("Promoted quote to policy from applicant: " <> applicantName k)

-- | Gets a policy from the storage.
getPolicy
  :: ( InsuranceStor Policy `Member` r
     , Error UseCaseError   `Member` r
     , Log                  `Member` r
     )
  => Applicant
  -> Sem r (Insurance Policy)
getPolicy k = whenNothingM (Storage.storageGet (Proxy :: Proxy (Insurance Policy)) k) $ 
  Error.throw (InsuranceDoesNotExists k)
