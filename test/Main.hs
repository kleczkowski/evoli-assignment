module Main (main) where

import Spec (allTests)

import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain allTests
