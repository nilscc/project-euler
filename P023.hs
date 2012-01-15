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
d 0 = 0
d n = sum $ divsOf n \\ [n]

abundants :: [Int]
abundants = [ x | x <- [1..28123], d x > x ]

sums :: [Int]
sums = sort [ a+b | a <- abundants, b <- abundants, a+b <= 28123 ]

go :: Int -> ([Int],[Int]) -> ([Int],[Int])
go x ([]   ,res) = ([], x:res)
go x ((y:s),res)
  | x == y       = (  s,  res)
  | otherwise    = (y:s,x:res)

main :: IO ()
main = print $ fst $ foldr go (sums, []) [1..28123]
