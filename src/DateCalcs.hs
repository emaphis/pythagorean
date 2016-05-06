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

calcSpread  :: Integer -> Int -> Int -> (Integer,Integer)
calcSpread yr mnth day = ((diffDays now1 jan01) + 1, diffDays dec31 now1)
  where now1  = fromGregorian yr mnth day
        jan01 = fromGregorian yr 1 1
        dec31 = fromGregorian yr 12 31

-- | Calculate the number of seconds given Hrs, Mns, Scs
calcSeconds ::  DiffTime -> DiffTime -> DiffTime -> DiffTime
calcSeconds hrs mns scs =   ((hrs*60*60) + (mns*60) + scs)


-- test for prime numbers
isPrime :: Integral a => a -> Bool
isPrime x = null (filter (\y ->  x `mod`y == 0) (takeWhile (\y ->  y*y <= x) [2..]))