-- | User friendly functions
--   This is essentially the user interface to the module

module Reports where

import Calcs
import DateCalcs


-- Some nicely formated reports

-- print lines and total of SE, PY, and HE
printGemTotal ::  String -> IO ()
printGemTotal txt = do
  printGemLines txt
  printGem txt

-- print one line for each Word of SE, PY and HE
printGemLines :: String -> IO ()
printGemLines txt = mapM_ printEach3 (findGem txt)

-- print total of SE, PY, and HE
printGem :: String -> IO ()
printGem txt = printEach3 (txt, sumSE txt, sumPY txt, sumHE txt)


printPyTotal :: String -> IO ()
printPyTotal txt = do
  printPyLines txt
  printPy txt

printPyLines :: String -> IO ()
printPyLines txt = mapM_ printEach3 (findPythag txt)

printPy :: String -> IO ()
printPy txt = printEach3 (txt, sumPY txt, sumPS txt, sumPX txt)


-- report funtions that also show work

-- print and show work given a calc funtion and a String
printSW :: (String -> [Int]) -> String -> IO()
printSW fn str = putStrLn  (str ++ " - " ++ showWList lst ++ " = " ++ show (sum lst))
  where lst = fn str


-- print and show work of various gematria
printSEsw,printPYsw,printPSsw,printPXsw,printHEsw
  :: String -> IO ()
printSEsw = printSW calcSE
printPYsw = printSW calcPY
printPSsw = printSW calcPS
printPXsw = printSW calcPX
printHEsw = printSW calcHE


-- print gematria totals and show work
printGemTotSW :: String -> IO ()
printGemTotSW str = do
  printSEsw str
  printPYsw str
  printHEsw str


-- print Pythagorean totals and show work
printPyTotSW :: String -> IO ()
printPyTotSW str = do
  printPYsw str
  printPSsw str
  printPXsw str


-- print ALL totals and show work
printTotSW :: String -> IO ()
printTotSW str = do
  printSEsw str
  printPYsw str
  printPSsw str
  printPXsw str
  printHEsw str


-- Date reports

printDateSplit :: (Integer, Int, Int) -> IO ()
printDateSplit (year, mnth, day) = do
  putStrLn (show mnth ++ "/" ++ show day ++ "/" ++ show yr ++ " - " ++
    showWList ones ++ " = " ++ show (sum ones))
  putStrLn (show mnth ++ "/" ++ show day ++ "/" ++ show yr ++ " - " ++
    showWList [mnth,day,hnds,tens] ++ " = " ++ show (sum [mnth,day,hnds,tens]))
  putStrLn (show mnth ++ "/" ++ show day ++ "/" ++ show tens ++ "   - " ++
    showWList [mnth,day,tens] ++ "   = " ++ show (sum [mnth,day,tens]))
  where yr = fromIntegral year
        ones = [spT mnth, spO mnth, spT day, spO day, spT hnds, spO hnds, spT tens, spO tens]
        hnds = yr `div` 100
        tens = yr `rem` 100
        spT n = n `div` 10
        spO n = n `rem` 10


printDateSpread :: (Integer,Int,Int) -> IO ()
printDateSpread (yr,mn,dy) = putStrLn (show d1 ++ " days since start of year " ++ show d2 ++ " days til end of year")
    where (d1,d2) = calcSpread (yr,mn,dy)


printDateDiff :: (Integer, Int, Int) -> (Integer, Int, Int) -> IO ()
printDateDiff (y1,m1,d1) (y2,m2,d2) =
  putStrLn (show dys ++ " days")
    where dys = calcDiffDays (y1,m1,d1) (y2,m2,d2)

-- Utility functions

printEach2 :: (String, Int, Int) -> IO ()
printEach2 (wrd, x, y) =
  putStrLn (wrd ++ " -- " ++ show x ++ "/" ++ show y)

printEach3 :: (String, Int, Int, Int) -> IO ()
printEach3 (wrd, x, y, z) =
  putStrLn (wrd ++ " -- " ++ show x ++ "/" ++  show y ++ "/" ++ show z)

-- show the work list
showWList :: (Eq a, Num a, Show a) => [a] -> String
showWList [x]     = show x
showWList (x:xs)
  | x == 0        = showWList xs
  | otherwise     =  show x ++ "+" ++ showWList xs
showWList []      = ""
