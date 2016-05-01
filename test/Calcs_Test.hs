-- | Unit testing geomatria calculation

module Calcs_Test where

import Tables
import Calcs
import Test.HUnit

-- test calcGem, calcSE

test_calcGem :: Test
test_calcGem = TestCase $ do
  assertEqual "calcGem base case"
    [] (calcGem seMap "")
  assertEqual "calGem on letter String"
    [1] (calcGem seMap "A")
  assertEqual "calcGem on 'balloon'"
    [2,1,12,12,15,15,14] (calcGem seMap "balloon")
  assertEqual "calcGem on all caps"
    [2,1,12,12,15,15,14] (calcGem seMap "BALLOON")
  assertEqual "calcGem using a py map"
    [2,1,3,3,6,6,5] (calcGem pyMap "balloon")

test_calcXX :: Test
test_calcXX = TestCase $ do
  assertEqual "test calcSE"
    [2,1,12,12,15,15,14] (calcSE "balloon")
  assertEqual "test calcPY"
    [2,1,3,3,6,6,5] (calcPY "balloon")
  assertEqual "test calcPY on 'juvenals'"
    [1,3,4,5,5,1,3,1] (calcPY "juvenals")
  assertEqual "test CalcPS on 'juvenals'"
    [1,3,4,5,5,1,3,10] (calcPS "juvenals")
  assertEqual "test calcPX on 'juvenals'"
    [1,3,22,5,5,1,3,10] (calcPX "juvenals")
  assertEqual "test caclHE"
    [2,1,20,20,50,50,40] (calcHE "balloon")



main :: IO Counts
main = runTestTT $ TestList [test_calcGem, test_calcXX]
