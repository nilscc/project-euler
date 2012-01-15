import Data.List

divsOf :: Int -> [Int]
divsOf n = nub {- . sort -} $ [1, n] ++ loop 2 (floor (sqrt (fi n)))
 where
  fi       = fromIntegral :: Int -> Double
  loop i s
    | i > s          = []
    | n `mod` i == 0 = [i, n`quot`i] ++ loop (i+1) s
                                     ++ loop (s+1) (floor (sqrt (fi (n`quot`i))))
    | otherwise      = loop (i+1) s

d :: Int -> Int
d n = sum $ divsOf n \\ [n]

go :: Int -> [Int] -> Int
go s []    = s
go s (a:r) =
  let b = d a
   in if a /= b && a == d b && a > 0 && b > 0
         then go (s+a+b) (delete b r)
         else go  s       r

main :: IO ()
main = print $ go 0 [1..10000]
