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

calcSpread :: String -> Maybe (Integer, Integer)
calcSpread day =
  case dt of
    Nothing              -> Nothing
    Just (yr, mnth, dy) -> Just (((diffDays now1 jan01) + 1, diffDays dec31 now1))
      where now1  = fromGregorian yr mnth dy
            jan01 = fromGregorian yr 1 1
            dec31 = fromGregorian yr 12 31
    where dt = conv (readDate day)


-- | convert a Maybe Day into a Maybe Triple
conv :: Maybe Day -> Maybe (Integer, Int, Int)
conv dy =  case dy of
        Nothing  -> Nothing
        Just d   -> Just (toGregorian d)


-- | Calculate the number of seconds given Hrs, Mns, Scs
calcSeconds ::  DiffTime -> DiffTime -> DiffTime -> DiffTime
calcSeconds hrs mns scs =   ((hrs*60*60) + (mns*60) + scs)


-- | Calculate the difference between two dates:
calcDiffDays :: String -> String -> Maybe Integer
calcDiffDays d1 d2 =
  case (readDate d1, readDate d2) of
    (Nothing  , _)         -> Nothing
    (_        , Nothing)   -> Nothing
    (Just day1, Just day2) -> Just (diffDays day1 day2)


-- | Parse DD/MM/YYYY into a Maybe Day
readDate    :: String -> Maybe Day
readDate str = parseTimeM True defaultTimeLocale "%m/%d/%Y" str

