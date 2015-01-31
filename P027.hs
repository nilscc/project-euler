{-# LANGUAGE ViewPatterns #-}

import Algorithm.MillerRabin

numberOfPrimes :: Int -> Int -> Int
numberOfPrimes (fromIntegral -> a) (fromIntegral -> b) = go 0
 where
  go n | isPrime (n^(2::Int) + a*n + b) = 1 + go (n+1)
       | otherwise                      = 0

factorsWithMaximumNumberOfPrimes
  :: (Int, Int)
factorsWithMaximumNumberOfPrimes =
   go (-999, -999, numberOfPrimes (-999) (-999))
      (-999)
      (-998)
 where
  go (a,b,_) 1000 1000 = (a,b)
  go (a,b,m) a'   1000 = go (a,b,m) (a'+1) (-999)
  go (a,b,m) a'   b'   =
    case numberOfPrimes a' b' of
      m' | m' > m    -> go (a',b',m') a' (b'+1)
         | otherwise -> go (a,b,m)    a' (b'+1)

main :: IO ()
main = do
  let r@(a,b) = factorsWithMaximumNumberOfPrimes
  putStrLn $ show r ++ " => " ++ show (a * b)
