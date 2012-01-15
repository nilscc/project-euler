import Data.List

go :: Int -> Int
go n
  | even n    = n `div` 2
  | otherwise = 3*n + 1

chainLength :: Int -> Int
chainLength n
  | n <= 1    = 1
  | otherwise = 1 + chainLength (go n)

p14 :: Int
p14 = fst $ foldl' findMax (0,0) [1..999999]
 where
  findMax r@(_,c) n
    | chainLength n > c = (n, chainLength n)
    | otherwise         = r

main :: IO ()
main = print p14
