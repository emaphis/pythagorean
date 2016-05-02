-- | Gematria calculations


module Calcs where

import Tables
import Data.Char (toUpper)
import Data.List.Split (splitOn)


-- calc gematria as a List of Int given a word and a GeMap
calcGem :: GemMap -> String -> [Int]
calcGem gMap str = map (getNum gMap . toUpper) str

-- calcGem seMap "balloon"
-- [2,1,12,12,15,15,14]
-- calcGem pyMap "balloon"
-- [2,1,3,3,6,6,5]


-- calculate Simple English, Pythagorean, Jewish, Pythagorean S,
-- Pythagorean Exceptions, Bacon
calcSE,calcPY,calcHE,calcPS,calcPX,calcBC
  :: String -> [Int]
calcSE = calcGem seMap
calcPY = calcGem pyMap
calcHE = calcGem heMap
calcPS = calcGem psMap
calcPX = calcGem pxMap
calcBC str =  map (getNum bcMap) str

-- > calcSE "balloon"
--   [2,1,12,12,15,15,14]
-- > calcPY "balloon"
--   [2,1,3,3,6,6,5]
-- > calcHE "balloon"
--   [2,1,20,20,50,50,40]


-- calculate the sum of Simple English, Simple Pythagorean, S Pythagorean,
-- Pathagorean with Exceptions, Jewish, Bacon
-- convenience function
sumSE,sumPY,sumHE,sumPS,sumPX,sumBC :: String -> Int
sumSE = sum . calcSE
sumPY = sum . calcPY
sumHE = sum . calcHE
sumPS = sum . calcPS
sumPX = sum . calcPX
sumBC = sum . calcBC

-- sumSE "balloon"
-- 71
-- sumPY "balloon"
-- 26


-- Some general purpose calculation functions
-- take a list of words and produce a list of Word,Gematria tuples
-- [String] -> Tuple

-- calculate the Simple English, Pythagorean, Jewish geometria of a list of words
calcGematria :: [String] -> [(String, Int, Int, Int)]
calcGematria = map (\s -> (s, sumSE s, sumPY s, sumHE s))

-- > calcGematria ["balloon"]
--   [("balloon",71,26,183)]
-- > calcGematria ["lets","go","on","a", "balloon", "ride"]
--   [("lets",56,11,215),("go",22,13,57),("on",29,11,90),("a",1,1,1),("balloon",71,26,183),("ride",36,27,98)]



-- calculate the Pythagorean Gematria of a list of words
calcPythag :: [String] -> [(String, Int, Int, Int)]
calcPythag = map (\s -> (s, sumPY s, sumPS s, sumPX s))

-- calcPythag ["lets","go","on","a", "balloon", "ride"]
-- [("lets",11,20,20),("go",13,13,13),("on",11,11,11),("a",1,1,1),("balloon",26,26,26),("ride",27,27,27)]
-- calcPythag ["Im","king", "of", "all", "I", "survey"]
-- [("Im",13,13,13),("king",23,23,32),("of",12,12,12),("all",7,7,7),("I",9,9,9),("survey",29,38,56)]


-- calculate the Bacon Gematria of a list of words
calcBacon :: [String] -> [(String, Int)]
calcBacon = map (\s -> (s, sumBC s))


-- Convenience funtions that take a String of text, chop it
-- up into words then calculate the gematria.
-- String -> Tuples

-- find the Simple English, Pythagorean, Jewish gematria of a given text
findGem :: String -> [(String, Int, Int, Int)]
findGem txt = calcGematria (getWords txt)

-- > findGem "lets go on a balloon ride"
--   [("lets",56,11,215),("go",22,13,57),("on",29,11,90),("a",1,1,1),("balloon",71,26,183),("ride",36,27,98)]


-- find the Simple and Exceptional Pythogorean gematria of a given text
findPythag :: String -> [(String, Int, Int, Int)]
findPythag txt = calcPythag (getWords txt)


-- given a String of text find the Bacon gematria
findBacon :: String -> [(String, Int)]
findBacon txt = calcBacon (getWords txt)


-- divide text into a list of words
getWords :: String -> [String]
getWords txt = splitOn " " txt

-- getWords "lets go on a balloon ride"
-- ["lets","go","on","a","balloon","ride"]
