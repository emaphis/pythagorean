-- | Number Calculations - Number Theory functions

module NumCalcs where

import Tables.PrimeTable (primeNumTbl)
import Data.List
import Data.Maybe (fromMaybe)


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


-- prime number lookup - faster than calculation

-- prime number table lookup
lookupPrime  :: Int -> Int
lookupPrime n = fromMaybe 0 (lookup n primeNumTbl)


-- lookup number of prime given that prime number
-- findIndex retruns the (index - 1)
lookupNumOfPrime :: Int -> Int
lookupNumOfPrime n = idx + 1
  where idx = fromMaybe (-1) (findIndex (\p -> snd p == n) primeNumTbl)


-- factors of an integer

factors  :: Int -> [Int]
factors n = filter (\d -> (n `rem` d) == 0) [1..n]


-- prime factors
primeFactors' :: Int -> [Int] -> [Int]
primeFactors' _ []  = []
primeFactors' n (x:xs)
   | x > n          = []
   | n `mod` x == 0 = x : primeFactors' (n `div` x) (x:xs)
   | otherwise      = primeFactors' n xs


primeFactors  :: Int -> [Int]
primeFactors n = primeFactors' n primes
