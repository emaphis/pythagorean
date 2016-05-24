-- | Tables used for text conversion

module Tables where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Char (toUpper, digitToInt, isDigit)

-- A table relating Char to Int
-- usefull for cipher calculations
type GemMap = M.Map Char Int

-- Simple English
seMap :: GemMap
seMap = M.fromList
  [('A', 1),
   ('B', 2),
   ('C', 3),
   ('D', 4),
   ('E', 5),
   ('F', 6),
   ('G', 7),
   ('H', 8),
   ('I', 9),
   ('J', 10),
   ('K', 11),
   ('L', 12),
   ('M', 13),
   ('N', 14),
   ('O', 15),
   ('P', 16),
   ('Q', 17),
   ('R', 18),
   ('S', 19),
   ('T', 20),
   ('U', 21),
   ('V', 22),
   ('W', 23),
   ('X', 24),
   ('Y', 25),
   ('Z', 26)]

-- English  (SE * 6)
enMap :: GemMap
enMap = M.fromList
  [('A', 6),
   ('B', 12),
   ('C', 18),
   ('D', 24),
   ('E', 30),
   ('F', 36),
   ('G', 42),
   ('H', 48),
   ('I', 54),
   ('J', 60),
   ('K', 66),
   ('L', 72),
   ('M', 78),
   ('N', 84),
   ('O', 90),
   ('P', 96),
   ('Q', 102),
   ('R', 108),
   ('S', 114),
   ('T', 120),
   ('U', 126),
   ('V', 132),
   ('W', 138),
   ('X', 144),
   ('Y', 150),
   ('Z', 156)]

-- Simple Pythagorean
pyMap :: GemMap
pyMap = M.fromList
  [('A', 1),
   ('B', 2),
   ('C', 3),
   ('D', 4),
   ('E', 5),
   ('F', 6),
   ('G', 7),
   ('H', 8),
   ('I', 9),
   ('J', 1),  -- 1+0
   ('K', 2),  -- 2/11 - 1+1 = 2
   ('L', 3),  -- 1+2
   ('M', 4),  -- 1+3
   ('N', 5),
   ('O', 6),
   ('P', 7),
   ('Q', 8),
   ('R', 9),  -- 1+8 = 9
   ('S', 1),  -- 1/10 - 1+9 = 10 = 1+0 = 1
   ('T', 2),  -- 2+0
   ('U', 3),  -- 2+1
   ('V', 4),  -- 4/22 - 2+2 = 4
   ('W', 5),  -- 2+3
   ('X', 6),  -- 2+4
   ('Y', 7),  -- 2+5
   ('Z', 8)]  -- 2+6

-- Pythagorean 'S' Exception
psMap :: GemMap
psMap = M.fromList
  [('A', 1),
   ('B', 2),
   ('C', 3),
   ('D', 4),
   ('E', 5),
   ('F', 6),
   ('G', 7),
   ('H', 8),
   ('I', 9),
   ('J', 1),  -- 1+0
   ('K', 2),  -- 2/11 - 1+1 = 2
   ('L', 3),  -- 1+2
   ('M', 4),  -- 1+3
   ('N', 5),
   ('O', 6),
   ('P', 7),
   ('Q', 8),
   ('R', 9),  -- 1+8 = 9
   ('S', 10), -- 1/10 - 1+9 = 10 = 1+0 = 1
   ('T', 2),  -- 2+0
   ('U', 3),  -- 2+1
   ('V', 4),  -- 4/22 - 2+2 = 4
   ('W', 5),  -- 2+3
   ('X', 6),  -- 2+4
   ('Y', 7),  -- 2+5
   ('Z', 8)]  -- 2+6

-- Pythagorean All Exceptions
pxMap :: GemMap
pxMap = M.fromList
  [('A', 1),
   ('B', 2),
   ('C', 3),
   ('D', 4),
   ('E', 5),
   ('F', 6),
   ('G', 7),
   ('H', 8),
   ('I', 9),
   ('J', 1),  -- 1+0
   ('K', 11), -- 2/11 - 1+1 = 2
   ('L', 3),  -- 1+2
   ('M', 4),  -- 1+3
   ('N', 5),
   ('O', 6),
   ('P', 7),
   ('Q', 8),
   ('R', 9),  -- 1+8 = 9
   ('S', 10), -- 1/10 - 1+9 = 10 = 1+0 = 1
   ('T', 2),  -- 2+0
   ('U', 3),  -- 2+1
   ('V', 22), -- 4/22 - 2+2 = 4
   ('W', 5),  -- 2+3
   ('X', 6),  -- 2+4
   ('Y', 7),  -- 2+5
   ('Z', 8)]  -- 2+6

-- Hebrew
heMap :: GemMap
heMap = M.fromList
  [('A', 1),
   ('B', 2),
   ('C', 3),
   ('D', 4),
   ('E', 5),
   ('F', 6),
   ('G', 7),
   ('H', 8),
   ('I', 9),
   ('J', 600),
   ('K', 10),
   ('L', 20),
   ('M', 30),
   ('N', 40),
   ('O', 50),
   ('P', 60),
   ('Q', 70),
   ('R', 80),
   ('S', 90),
   ('T', 100),
   ('U', 200),
   ('V', 700),
   ('W', 900),
   ('X', 300),
   ('Y', 400),
   ('Z', 500)]

-- Clasic English - "The English Gematria System",
ceMap :: GemMap
ceMap = M.fromList
  [('A', 1),
   ('B', 2),
   ('C', 3),
   ('D', 4),
   ('E', 5),
   ('F', 6),
   ('G', 7),
   ('H', 8),
   ('I', 9),
   ('J', 10),
   ('K', 20),
   ('L', 30),
   ('M', 40),
   ('N', 50),
   ('O', 60),
   ('P', 70),
   ('Q', 80),
   ('R', 90),
   ('S', 100),
   ('T', 200),
   ('U', 300),
   ('V', 400),
   ('W', 500),
   ('X', 600),
   ('Y', 700),
   ('Z', 800)]


-- Bacon Gematria
-- a-z = 1-26
-- A-Z = 27-52
bcMap :: GemMap
bcMap = M.fromList
  [('a', 1),
   ('b', 2),
   ('c', 3),
   ('d', 4),
   ('e', 5),
   ('f', 6),
   ('g', 7),
   ('h', 8),
   ('i', 9),
   ('j', 10),
   ('k', 11),
   ('l', 12),
   ('m', 13),
   ('n', 14),
   ('o', 15),
   ('p', 16),
   ('q', 17),
   ('r', 18),
   ('s', 19),
   ('t', 20),
   ('u', 21),
   ('v', 22),
   ('w', 23),
   ('x', 24),
   ('y', 25),
   ('z', 26),
   ('A', 27),
   ('B', 28),
   ('C', 29),
   ('D', 30),
   ('E', 31),
   ('F', 32),
   ('G', 33),
   ('H', 34),
   ('I', 35),
   ('J', 35),
   ('K', 37),
   ('L', 38),
   ('M', 39),
   ('N', 40),
   ('O', 41),
   ('P', 42),
   ('Q', 43),
   ('R', 44),
   ('S', 45),
   ('T', 46),
   ('U', 47),
   ('V', 48),
   ('W', 49),
   ('X', 50),
   ('Y', 51),
   ('Z', 52)]


-- get an Int from a GenMap given a Char
-- strip the Maybe default to 0
getNum :: GemMap -> Char -> Int
getNum gMap c
  | isDigit c  = digitToInt c
  | otherwise  = fromMaybe 0 (M.lookup c gMap)

-- convert char to upper before calculation
getNumLower :: GemMap -> Char -> Int
getNumLower gMap c = getNum gMap (toUpper c)


-- look up letter given a number and a GemMap
getLetter ::  Int -> GemMap -> Char
getLetter n = head . M.keys . M.filter (==n)


getLetterSE :: Int -> Char
getLetterSE n = getLetter n seMap
