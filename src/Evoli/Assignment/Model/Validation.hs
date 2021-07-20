{-# LANGUAGE RecordWildCards #-}
module Evoli.Assignment.Model.Validation where

import Evoli.Assignment.Model.Types

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Calendar
import Data.Validation

data QuoteValidationError
  = InvalidItemPrice !Rational
  | InvalidDateRange !Day !Day
  | EmptyApplicantName !Applicant
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

validateQuote :: Insurance Quote -> Validation [QuoteValidationError] (Insurance Quote)
validateQuote Insurance{..} =
  Insurance{..}
    <$ checkDayRange insuranceStartDate insuranceEndDate
    <* checkItemPrice insuredItemPrice
  where
    checkDayRange start end
      | start > end = Failure [InvalidDateRange start end]
      | otherwise = Success ()
    checkItemPrice price
      | price <= 0 = Failure [InvalidItemPrice price]
      | otherwise = Success ()
