import Data.Numbers.Fibonacci

main :: IO ()
main = print $ head $ dropWhile (\n -> length (show (fib n)) < 1000) [1..]
