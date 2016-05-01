-- | Gematria calculations


module Calcs where

import Tables
import Data.Char (toUpper)
-- import qualified Data.Map as M -- temp

-- calc gematria as a List of Int given a word and a GeMap
calc :: GemMap -> String -> [Int]
calc gMap str = map (getNum gMap . toUpper) str

-- calc seMap "balloon"
-- [2,1,12,12,15,15,14]
-- calc pyMap "balloon"
-- [2,1,3,3,6,6,5]
