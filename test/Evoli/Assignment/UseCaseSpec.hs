{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Evoli.Assignment.UseCaseSpec (spec) where

import Prelude hiding (State, get, modify, modify', runState)

import Evoli.Assignment.Interfaces.Storage
import Evoli.Assignment.Model
import Evoli.Assignment.UseCase

import qualified Data.Map.Strict as M
import Data.Time.Calendar
import Polysemy
import Polysemy.Error
import Polysemy.Log
import Polysemy.State
import Test.Tasty
import Test.Tasty.HUnit
import Unsafe.Coerce


spec :: TestTree
spec = testGroup "Use-case tests"
  [ createQuoteSpec
  , discardQuoteSpec
  , acceptQuoteSpec
  , getPolicySpec
  ]

createQuoteSpec :: TestTree
createQuoteSpec = testGroup "createQuote"
  [ testCase "default case" $ do
      let applicant = Applicant "abc"
          quote = Insurance
              { insuranceStartDate = fromGregorian 2021 7 21
              , insuranceEndDate = fromGregorian 2021 7 30
              , insuredItemPrice = 10.75
              }
      case runPure (createQuote applicant quote) of
        Left e -> assertFailure (show e)
        Right (_, (m, InsurancePrice cost)) -> do
          assertBool "storage contains quote" (applicant `M.member` m)
          assertBool "valid cost has been computed" (cost == 0.2 * insuredItemPrice quote)
  , testGroup "validation tests"
      [ testCase "invalid time range" $ do
          let applicant = Applicant "abc"
              quote = Insurance
                  { insuranceEndDate = fromGregorian 2021 7 21
                  , insuranceStartDate = fromGregorian 2021 7 30
                  , insuredItemPrice = 10.75
                  }
          case runPure (createQuote applicant quote) of
            Left (QuoteValidationErrors [InvalidDateRange _ _]) -> pass
            _ -> assertFailure "InvalidDataRange error should happen"
      , testCase "empty applicant" $ do
          let applicant = Applicant ""
              quote = Insurance
                  { insuranceStartDate = fromGregorian 2021 7 21
                  , insuranceEndDate = fromGregorian 2021 7 30
                  , insuredItemPrice = 10.75
                  }
          case runPure (createQuote applicant quote) of
            Left (QuoteValidationErrors [EmptyApplicantName _]) -> pass
            _ -> assertFailure "EmptyApplicantName error should happen"
      , testCase "negative item price" $ do
          let applicant = Applicant "abc"
              quote = Insurance
                  { insuranceStartDate = fromGregorian 2021 7 21
                  , insuranceEndDate = fromGregorian 2021 7 30
                  , insuredItemPrice = 0
                  }
          case runPure (createQuote applicant quote) of
            Left (QuoteValidationErrors [InvalidItemPrice _]) -> pass
            _ -> assertFailure "InvalidItemPrice error should happen"
      ]
  , testGroup "storage tests"
    [ testCase "quote in storage" $ do
      let applicant = Applicant "abc"
          quote = Insurance
              { insuranceStartDate = fromGregorian 2021 7 21
              , insuranceEndDate = fromGregorian 2021 7 30
              , insuredItemPrice = 10.75
              }
      case runPure (createQuote applicant quote >> createQuote applicant quote) of
        Left (InsuranceAlreadyExists _) -> pass
        _ -> assertFailure "InsuranceAlreadyExists error should happen"
    ]
  ]

discardQuoteSpec :: TestTree
discardQuoteSpec = testGroup "discardQuote"
  [ testCase "default case" $ do
      let applicant = Applicant "abc"
          quote = Insurance
              { insuranceStartDate = fromGregorian 2021 7 21
              , insuranceEndDate = fromGregorian 2021 7 30
              , insuredItemPrice = 10.75
              }
      case runPure (createQuote applicant quote >> discardQuote applicant) of
        Left e -> assertFailure (show e)
        Right (_, (m, _)) -> do
          assertBool "store should not contain quote" (applicant `M.notMember` m)
  , testCase "discarding non-existing quote" $ do
      let applicant = Applicant "abc"
      case runPure (discardQuote applicant) of
        Left (InsuranceDoesNotExists _) -> pass
        _ -> assertFailure "InsuranceDoesNotExists error should happen"
  ]

acceptQuoteSpec :: TestTree
acceptQuoteSpec = testGroup "acceptQuote"
  [ testCase "default case" $ do
      let applicant = Applicant "abc"
          quote = Insurance
              { insuranceStartDate = fromGregorian 2021 7 21
              , insuranceEndDate = fromGregorian 2021 7 30
              , insuredItemPrice = 10.75
              }
      case runPure (createQuote applicant quote >> acceptQuote applicant) of
        Left e -> assertFailure (show e)
        Right (m, (n, _)) -> do
          assertBool "quote store should not contain quote" (applicant `M.notMember` n)
          assertBool "policy store should contain quote" (applicant `M.member` m)
  , testCase "existing policy" $ do
      let applicant = Applicant "abc"
          quote = Insurance
              { insuranceStartDate = fromGregorian 2021 7 21
              , insuranceEndDate = fromGregorian 2021 7 30
              , insuredItemPrice = 10.75
              }
      case runPure (createQuote applicant quote >> acceptQuote applicant >> createQuote applicant quote >> acceptQuote applicant) of
        Left (InsuranceAlreadyExists _) -> pass
        _ -> assertFailure "InsuranceAlreadyExists error should happen"
  ]

getPolicySpec :: TestTree
getPolicySpec = testGroup "getPolicy"
  [ testCase "default case" $ do
      let applicant = Applicant "abc"
          quote = Insurance
              { insuranceStartDate = fromGregorian 2021 7 21
              , insuranceEndDate = fromGregorian 2021 7 30
              , insuredItemPrice = 10.75
              }
      case runPure (createQuote applicant quote >> acceptQuote applicant >> getPolicy applicant) of
        Left e -> assertFailure (show e)
        Right (m, (n, a)) -> do
          assertBool "policy equals quote" (toInsurancePolicy quote == a)
  ]

runTestStore
  :: forall k v r a
   . Ord k
  => Sem (Storage k v ': r) a
  -> Sem (State (M.Map k v) ': r) a
runTestStore = reinterpret $ \case
  StorageGet _ k    -> M.lookup k <$> get
  StoragePut k v    -> modify (M.insert k v :: Map k v -> Map k v)
  StorageDelete _ k -> modify (M.delete k :: Map k v -> Map k v)

runPure
  :: Sem '[InsuranceStor Quote, InsuranceStor Policy, Error UseCaseError, Log] a
  -> Either UseCaseError (Map Applicant (Insurance Policy), (Map Applicant (Insurance Quote), a))
runPure s = s
  & runTestStore
  & runState M.empty
  & runTestStore
  & runState M.empty
  & runError @UseCaseError
  & interpretLogNull
  & run
