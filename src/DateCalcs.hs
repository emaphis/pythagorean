-- | Date and Time calculations

module DateCalcs where

import Control.Monad
import Data.Time


-- formating experiments
format1 :: FormatTime t => t -> String
format1 = formatTime defaultTimeLocale "%Y-%m-%e"
 
format2 :: FormatTime t => t -> String
format2 = formatTime defaultTimeLocale "%A, %B %d, %Y"
 
main :: IO ()
main = do
    t <- liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime
    mapM_ putStrLn [format1 t, format2 t]



-- | Calculate the days from beginning of year to now
--   and days until end of year

calcSpread  :: (Integer,Int,Int) -> (Integer,Integer)
calcSpread (yr, mnth, day) =
  ((diffDays now1 jan01) + 1, diffDays dec31 now1)
  where now1  = fromGregorian yr mnth day
        jan01 = fromGregorian yr 1 1
        dec31 = fromGregorian yr 12 31

-- | Calculate the number of seconds given Hrs, Mns, Scs
calcSeconds ::  DiffTime -> DiffTime -> DiffTime -> DiffTime
calcSeconds hrs mns scs =   ((hrs*60*60) + (mns*60) + scs)


-- | Calculate the difference between two dates:
calcDiffDays :: (Integer, Int, Int) -> (Integer, Int, Int) -> Integer
calcDiffDays (y1,m1,d1) (y2,m2,d2) = diffDays day1 day2
  where day1 = fromGregorian y1 m1 d1
        day2 = fromGregorian y2 m2 d2

-- | Parse DD/MM/YYY into a Day
parseDate  :: String -> Day
parseDate s = readTime defaultTimeLocale "%/m%/%Y" s

-- parseTimeOrError True

-- test for prime numbers
isPrime :: Integral a => a -> Bool
isPrime x =  not (any (\y ->  x `mod`y == 0) (takeWhile (\y ->  y*y <= x) [2..]))

-- produce a lazy list of primes
primes :: [Int]
primes = 2 : filter isPrime [3, 5..]


-- produce an indexed list of primes upto a given number
primeList  :: Int -> [(Int,Int)]
primeList n = take n (zip [1..] primes)


-- produce the Nth prime number
primeNth  :: Int -> Int
primeNth n = primes !! (n-1)

primesOf11 :: [(Int,Int)]
primesOf11 =
  [(11, 29),
   (22, 73),
   (33, 131),
   (44, 191),
   (55, 251),
   (66, 313),
   (77, 383),
   (88, 449),
   (99, 521),
   (110, 599),
   (121, 659)]
