{-# LANGUAGE BangPatterns #-}

{- | Miller-Rabin primality test
 -
 -}
module Algorithm.MillerRabin where

import System.Random

import Algorithm.PowMod

-- testing
import Data.List
import Test.HUnit

factoringPowers :: Integer -> (Integer, Integer)
factoringPowers n = loop (n-1) 0
 where
  loop d s
    | even d    = loop (d `div` 2) (s+1)
    | otherwise = (s, d)

data MillerRabinResult
  = Prime
  | ProbablyPrime
  | Composite
  deriving (Eq, Show)

millerRabin
  :: RandomGen g
  => Integer -- ^ n > 3, odd integer to be tested for primality
  -> Int     -- ^ k, parameter that determines the accuracy of the test
  -> g
  -> (g, MillerRabinResult)

millerRabin n _ g
  | n <= 3 || even n = (g, Composite)

millerRabin n k gen = witnessLoop k gen
 where
  (s, d) = factoringPowers n

  witnessLoop 0 g = (g, ProbablyPrime)
  witnessLoop i g
    | x == 1 || x == n-1 = witnessLoop (i-1) g'
    | otherwise          = innerLoop (s-1) x
   where
    (a, g') = randomR (2, n - 2) g
    x       = powMod a d n

    innerLoop 0 _ = (g', Composite)
    innerLoop j x'
      | x'' == 1   = (g', Composite)
      | x'' == n-1 = witnessLoop (i-1) g'
      | otherwise = innerLoop (j - 1) x'
     where
      x'' = powMod x' 2 n

-- | Deterministic Miller-Rabin primality test
detMillerRabin
  :: Integer      -- ^ n > 1, an odd integer to test for primality
  -> MillerRabinResult
detMillerRabin n
  | n < 1 || (n > 2 && even n) = Composite
  | n == 1 || n == 2 = Prime
  | n < 2047 = loop [2]
  | n < 1373653 = loop [2,3]
  | n < 9080191 = loop [31,73]
  | n < 25326001 = loop [2,3,5]
  | n < 4759123141 = loop [2,7,61]
  | n < 1122004669633 = loop [2,13,23,1662803]
  | n < 2152302898747 = loop [2,3,5,7,11]
  | n < 3474749660383 = loop [2,3,5,7,11,13]
  | n < 341550071728321 = loop [2,3,5,7,11,13,17]
  | n < 3825123056546413051 = loop [2,3,5,7,11,13,17,19,23]
  | otherwise = loop [ 2 .. min (n-1) (floor $ 2 * (log n')^(2 :: Int)) ]
 where
  n' = fromIntegral n :: Double

  (s, d) = factoringPowers n

  loop [] = Prime
  loop (a:as)
    | (powMod a d n) /= 1 && powLoop 0
    = Composite

    | otherwise
    = loop as
   where
    powLoop r
      | r < s     = (powMod a (2^r * d) n) /= (n-1) && powLoop (r+1)
      | otherwise = True

isPrime :: Integer -> Bool
isPrime n = detMillerRabin n == Prime

------------------------------------------------------------------------------
-- Testing
--

tests :: [Test]
tests =
  map (expect True) primes
  ++
  map (expect False) notPrimes
 where
  expect val n = " " ++ show n ~: val ~=? isPrime n

  primes = [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59,
    61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 7649, 7669, 7673,
    7681, 7687, 7691, 7699, 7703, 7717, 7723, 7727, 7741, 7753, 7757, 7759,
    7789, 7793, 7817, 7823, 7829, 7841, 7853, 7867, 7873, 7877, 7879, 7883,
    7901, 7907, 7919]

  notPrimes = nub [341, 91, 121, 15, 85, 91, 435, 451, 217, 35, 185, 217, 301,
    481, 25, 325, 9, 21, 45, 63, 65, 105, 117, 133, 153, 231, 274, 341, 481,
    92, 121, 205, 9, 33, 91, 99, 259, 451, 481, 15, 133, 259, 305, 481, 65, 91,
    133, 143, 145, 247, 377, 385, 21, 85, 105, 231, 357, 427, 15, 39, 65, 195,
    481, 341, 15, 51, 85, 91, 255, 341, 435, 451, 9, 45, 91, 145, 261, 25, 49,
    65, 85, 133, 221, 323, 325, 343, 425, 451, 9, 15, 45, 49, 153, 169, 343,
    21, 57, 133, 231, 399]

runTests :: IO Counts
runTests = runTestTT $ TestList tests
