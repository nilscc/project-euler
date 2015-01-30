module Algorithm.PowMod where

import Prelude hiding (exp)
import Data.Bits

-- | Calculate b^e `mod` m
powMod
  :: Integer  -- ^ Base b
  -> Integer  -- ^ Exponent e
  -> Integer  -- ^ Modulus m
  -> Integer
powMod b e m = loop 1 (b `mod` m) e
 where
  loop res base exp
    | exp <= 0 = res
    | otherwise =
      let res'  = if exp `mod` 2 == 1 then (res * base) `mod` m else res
          exp'  = exp `shift` (-1)
          base' = (base * base) `mod` m
      in loop res' base' exp'
