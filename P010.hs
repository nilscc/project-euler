start :: Int -> Int
start x = ceiling . sqrt . fi $ x
 where
  fi :: Int -> Double
  fi = fromIntegral

divsOf :: Int -> [Int]
divsOf x = filter (\y -> x `mod` y == 0)
                  [s, s-1 .. 2]
 where
  s = start x

isPrim :: Int -> Bool
isPrim x = null divs || divs == [x]
 where
  divs = divsOf x

--------------------------------------------------------------------------------
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
--
--------------------------------------------------------------------------------

p10 :: Int
p10 = sum $ filter isPrim [2..2000000]

main :: IO ()
main = print p10
