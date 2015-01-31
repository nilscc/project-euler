isDigitPower
  :: Integer -- ^ Exponent
  -> Integer -- ^ Number
  -> Bool
isDigitPower e n = n == sum [ d^e | d <- digits n ]

digits :: Integer -> [Integer]
digits x
  | x > 0     = dropWhile (== 0) $
                digits (x `div` 10) ++ [fromIntegral $ x `mod` 10]
  | otherwise = [0]

digitPowersOf5 :: [Integer]
digitPowersOf5 = filter (isDigitPower 5) [10..1000000]

main :: IO ()
main = print $ sum $ digitPowersOf5
