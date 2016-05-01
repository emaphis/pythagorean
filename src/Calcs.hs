-- | Gematria calculations


module Calcs where

import Tables
import Data.Char (toUpper)


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
