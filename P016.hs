{-# OPTIONS -fno-warn-type-defaults #-}

main :: IO ()
main = print $ sum . toDigits $ 2^1000

toDigits :: Integer -> [Int]
toDigits = map (read . return) . show
