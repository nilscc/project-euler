import Data.List

data Dec
  = Dec { dDiv :: Int, dRem :: Int }
  | Rec [Int]
  deriving Eq

instance Show Dec where
  show (Dec i _) = show i
  show (Rec r  ) = "(" ++ concatMap show r ++ ")"

data Exact = Exact Int [Dec]

instance Show Exact where
  show (Exact i ds) = show i ++ "." ++ if null ds then "0" else concatMap show ds

exact :: Int -> Int -> Exact -- (Int,[Dec])
exact x y = Exact (x `div` y) (go ((x `rem` y)*10) [])
 where
  go :: Int -> [Dec] -> [Dec]
  go 0  decs = decs
  go x' decs =
    let d = x' `div` y
        r = x' `rem` y
     in if Dec d r `elem` decs
           then mkRec $ span (Dec d r /=) decs
           else go (r*10) (decs ++ [Dec d r])
  mkRec (ds,rs) = ds ++ [Rec (map (\(Dec i _) -> i) rs)]

lengthOfRec :: [Dec] -> Int
lengthOfRec []          = 0
lengthOfRec (Dec _ _ : r) = lengthOfRec r
lengthOfRec (Rec r   : _) = length r

main :: IO ()
main = print $
  maximumOn snd [ (d,lengthOfRec ds) | d <- [1..1000], let Exact _ ds = exact 1 d ]

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (\x y -> f x `compare` f y)
