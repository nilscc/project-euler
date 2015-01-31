import Algorithm.MillerRabin

primes :: [Integer]
primes = filter isPrime [2..]

goal :: Integer
goal = 1000000

consecutiveSum :: (Integer, Int)
consecutiveSum = go 2 0 (0,0)
 where
  end = length $ takeWhile (< goal) primes

  go i j res@(_,l)
    | s < goal
    = go (i+1) j (if isPrime s && i > l then (s,i) else res)

    | (i+j) <= end
    = go 2 (j+1) res

    | otherwise = res

   where
    s = sum $ take i $ drop j primes

main :: IO ()
main = print consecutiveSum
