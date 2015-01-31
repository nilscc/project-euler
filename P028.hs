import Test.HUnit

spiralSum :: Integer -> Integer
spiralSum l = go 1 2
 where
  go s step
    | step < l  = go (s + 4 * (step-1)^2 + 10 * step)
                     (step+2)
    | otherwise = s

main :: IO ()
main = do
  print =<< runTests
  print $ spiralSum 1001

runTests :: IO Counts
runTests = runTestTT $ TestList
  [ 101 ~=? spiralSum 5
  ]
