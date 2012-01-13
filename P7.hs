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
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6th prime is 13.
--
-- What is the 10 001st prime number?
--
--------------------------------------------------------------------------------

sixth_prime :: Int
sixth_prime = last $ take 6 $ filter isPrim [1..]

p7 :: Int
p7 = last $ take 10001 $ filter isPrim [1..]
