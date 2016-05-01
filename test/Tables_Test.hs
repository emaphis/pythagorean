-- | Testing Tables

module Tables_Test where

import Tables
import Test.HUnit
import Data.Char (toUpper)


getNumCases :: Test
getNumCases = TestList [testGetNum, testGetNumLower]

testGetNum, testGetNumLower :: Test

testGetNum = TestCase $ do
  assertEqual "Missing data should return 0"
    0 (getNum seMap 'a')
  assertEqual "'A' should return 1"
    1 (getNum seMap 'A')
  assertEqual "(toUpper 'a') should return 1"
    1 (getNum seMap (toUpper 'a'))
  assertEqual "'.' should return 0"
    0 (getNum seMap '.')
  assertEqual "'?' should return 0"
    0 (getNum seMap '?')

testGetNumLower = TestCase $ do
  assertEqual "Small letters should return data"
    1 (getNumLower seMap 'a')
  assertEqual "Capital letters should return data"
    1 (getNumLower seMap 'A')


main :: IO Counts
main = runTestTT $ TestList [getNumCases]
