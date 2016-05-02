-- | Unit testing geomatria calculation

module Calcs_Test where

import Tables
import Calcs
import Test.HUnit

-- test calcGem, calcXX, sumXX

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

test_sumXX :: Test
test_sumXX = TestCase $ do
  assertEqual "test sumSE on balloon"
    71 (sumSE "balloon")
  assertEqual "2+1+12+15+15+14"
    71 (2+1+12+12+15+15+14)
  assertEqual "test sumPY"
    26 (sumPY "balloon")

test_calcGematria :: Test
test_calcGematria = TestCase $ do
  assertEqual "test calGem base case"
    [] (calcGematria [])
  assertEqual "test 'balloon'"
    [("balloon",71,26,183)] (calcGematria ["balloon"])
  assertEqual "lets go on a balloon ride"
     [("lets",56,11,215),("go",22,13,57),("on",29,11,90),("a",1,1,1),("balloon",71,26,183),("ride",36,27,98)]
     (calcGematria ["lets","go","on","a", "balloon", "ride"])


test_calcPythag :: Test
test_calcPythag = TestCase $ do
  assertEqual "test calcPythag base case"
    [] (calcPythag [])
  assertEqual "test one word"
    [("survey",29,38,56)] (calcPythag ["survey"])
  assertEqual "lets go on a balloon ride"
    [("lets",11,20,20),("go",13,13,13),("on",11,11,11),("a",1,1,1),("balloon",26,26,26),("ride",27,27,27)]
    (calcPythag ["lets","go","on","a", "balloon", "ride"])
  assertEqual "Im king of all I servey"
    [("Im",13,13,13),("king",23,23,32),("of",12,12,12),("all",7,7,7),("I",9,9,9),("survey",29,38,56)]
    (calcPythag ["Im","king", "of", "all", "I", "survey"])

test_findGem :: Test
test_findGem = TestCase $ do
  assertEqual "findGem base case"
    [("",0,0,0)] (findGem "")
  assertEqual "test findGem 'lets go on a ballon ride'"
    (findGem "lets go on a balloon ride")
    [("lets",56,11,215),("go",22,13,57),("on",29,11,90),("a",1,1,1),("balloon",71,26,183),("ride",36,27,98)]

test_findPythag :: Test
test_findPythag = TestCase $ do
  assertEqual "findPythag base case"
    [("",0,0,0)]  (findPythag "")
  assertEqual "test 'lets go on a balloon ride"
    [("lets",11,20,20),("go",13,13,13),("on",11,11,11),("a",1,1,1),("ballon",20,20,20),("ride",27,27,27)]
    (findPythag "lets go on a ballon ride")
  assertEqual "test Im the king of all I servey"
    [("Im",13,13,13),("the",15,15,15),("king",23,23,32),("of",12,12,12),("all",7,7,7),("I",9,9,9),("survey",29,38,56)]
    (findPythag "Im the king of all I survey")


test_getWords :: Test
test_getWords = TestCase $ do
  assertEqual "getWords base case"
    [""] (getWords "")
  assertEqual "lets go on a ballon ride"
    ["lets","go","on","a","balloon","ride"]
    (getWords "lets go on a balloon ride")


main :: IO Counts
main = runTestTT $ TestList
  [test_calcGem, test_calcXX, test_sumXX, test_calcGematria,
   test_calcPythag, test_getWords, test_findGem, test_findPythag]
