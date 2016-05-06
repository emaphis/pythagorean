-- | User friendly functions
--   This is essentially the user interface to the module

module Reports where

import Calcs


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


-- print ALL totalss and show work
printTotSW :: String -> IO ()
printTotSW str = do
  printSEsw str
  printPYsw str
  printPSsw str
  printPXsw str
  printHEsw str


-- Utility functions

printEach2 :: (String, Int, Int) -> IO ()
printEach2 (wrd, x, y) =
  putStrLn (wrd ++ " -- " ++ show x ++ "/" ++ show y)

printEach3 :: (String, Int, Int, Int) -> IO ()
printEach3 (wrd, x, y, z) =
  putStrLn (wrd ++ " -- " ++ show x ++ "/" ++  show y ++ "/" ++ show z)

-- show the work list
showWList :: Show a => [a] -> String
showWList [x]     = show x
showWList (x:xs)  = show x ++ "+" ++ showWList xs
showWList []      = ""
