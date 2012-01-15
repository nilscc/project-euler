toDigits :: Integer -> [Int]
toDigits = map (read . return) . show

fac :: Int -> Integer
fac x = product [1..fromIntegral x]

main :: IO ()
main = print . sum . toDigits $ fac 100
