module Main where

import qualified Heap.Leftist as HL
import qualified Heap.Weighted as HW
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "okasaki"
  [ testGroup "leftist heap" HL.tests
  , testGroup "weight-biased leftist heap" HW.tests
  ]
