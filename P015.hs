routes :: Integer -> Integer
routes n = (2*n) `binom` n

-- binomial coefficient for positive integers
binom :: Integer -> Integer -> Integer
binom n k = floor $ fac n / (fac k * fac (n-k))
 where
  fac x = fromIntegral $ product [1..x] :: Double

main :: IO ()
main = print (routes 20)
