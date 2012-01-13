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


------------------------------------------------------------
-- Problem 3:
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?
--
------------------------------------------------------------

-- example testing/verifying
p3_ex :: Int
p3_ex = 13195

p3_ex_res :: [Int]
p3_ex_res = [29,13,7,5]

p3_ex_correct :: Bool
p3_ex_correct = p3_ex_res == filter isPrim (divsOf p3_ex) -- > True

-- the actual problem
p3 :: Int
p3 = 600851475143

p3_res :: Int
p3_res = head $ filter isPrim (divsOf p3)
