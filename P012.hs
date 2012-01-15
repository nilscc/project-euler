import Data.List (nub)

divsOf :: Int -> [Int]
divsOf n = nub {- . sort -} $ [1, n] ++ loop 2 (floor (sqrt (fi n)))
 where
  fi       = fromIntegral :: Int -> Double
  loop i s
    | i > s          = []
    | n `mod` i == 0 = [i, n`quot`i] ++ loop (i+1) s
                                     ++ loop (s+1) (floor (sqrt (fi (n`quot`i))))
    | otherwise      = loop (i+1) s

-- infinit list of triangle numbers
triangs :: [Int]
triangs = [ sum [1..x] | x <- [1..] ]

main :: IO ()
main = mapM_ print $ count 0 triangs
 where
  count _ []     = []
  count c (x:xs) | d >= 500  = (x,d) : []
                 | d > c     = (x,d) : count d xs
                 | otherwise =         count c xs
   where
    d = length (divsOf x)
