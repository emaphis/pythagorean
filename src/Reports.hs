-- | User friendly functions
--   This is essentially the user interface to the module

module Reports where

import Calcs
import DateCalcs
import NumCalcs ()


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


-- print totals
printTot :: String -> IO ()
printTot txt = do
  putStrLn (txt ++ " -- " ++
            show (sumSE txt) ++ " SE - " ++ show (sumEN txt) ++ " EN - " ++
            show (sumCE txt) ++ " CE - " ++ show (sumPY txt) ++ " PY - " ++
            show (sumPS txt) ++ " PS - " ++ show (sumPX txt) ++ " PX - " ++
            show (sumHE txt) ++ " HE - " ++ show (sumBC txt) ++ " BC ")


-- report funtions that also show work

-- print and show work given a calc funtion and a String
printSW :: (String -> [Int]) -> String -> IO ()
printSW fn str = putStrLn  (str ++ " - " ++ showWList lst ++ " = " ++ show (sum lst))
  where lst = fn str


-- print and show work of various gematria
printSEsw,printPYsw,printPSsw,printPXsw,printHEsw,printBCsw
  :: String -> IO ()
printSEsw = printSW calcSE
printPYsw = printSW calcPY
printPSsw = printSW calcPS
printPXsw = printSW calcPX
printHEsw = printSW calcHE
printBCsw = printSW calcBC


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
  printBCsw str


-- Date reports

printDateSplit :: String -> IO ()
printDateSplit day1 = do
  case dt of
    Nothing                -> putStrLn  "Error in date format: Should be MM/DD/YYYY"
    Just (year,month,day)  -> do
      putStrLn (show month ++ "/" ++ show day ++ "/" ++ show yr ++ " - " ++
        showWList ones ++ " = " ++ show (sum ones))
      putStrLn (show month ++ "/" ++ show day ++ "/" ++ show yr ++ " - " ++
        showWList [month,day,hnds,tens] ++ " = " ++ show (sum [month,day,hnds,tens]))
      putStrLn (show month ++ "/" ++ show day ++ "/" ++ show tens ++ "   - " ++
        showWList [month,day,tens] ++ "   = " ++ show (sum [month,day,tens]))
      where yr = fromIntegral year
            ones = [spT month, spO month, spT day, spO day, spT hnds, spO hnds, spT tens, spO tens]
            hnds = yr `div` 100
            tens = yr `rem` 100
            spT n = n `div` 10
            spO n = n `rem` 10
    where  dt = conv (readDate day1)

printDateSpread :: String -> IO ()
printDateSpread day =
  case dt of
    Nothing       -> putStrLn  "Error in date format: Should be MM/DD/YYYY"
    Just (d1, d2) ->
      putStrLn (show d1 ++ " days since start of year " ++ show d2 ++ " days til end of year")
    where dt = calcSpread day


printDateDiff :: String -> String -> IO ()
printDateDiff day1 day2 =
  case dt of
    Nothing      -> putStrLn  "Error in date format: Should be MM/DD/YYYY"
    Just dys     -> putStrLn (show dys ++ " days")
   where dt = calcDiffDays day1 day2


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
