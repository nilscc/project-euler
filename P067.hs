
import qualified Data.Map as M
import Data.Map (elems, Map, alter, filterWithKey, fromList)

import P067data

type Coord = (Int,Int)

-- the "weight" is simply the value at the given position
weight :: Coord -> Int
weight (x,y) = (triang !! x) !! ((y+x)`div`2)

done :: Coord -> Bool
done (x,_) = x == length triang - 1

--------------------------------------------------------------------------------
--
-- the longest path algorithm
--
--------------------------------------------------------------------------------

type LengthMap = Map Coord Int

go :: Int
go = go' 0 (fromList [((0,0), weight (0,0))])

go' :: Int -> LengthMap -> Int
go' x lengthTo
  | done (x,0) = maximum (elems (filterWithKey currentLine lengthTo))
  | otherwise  = go' (x+1) $ foldr goOne lengthTo coords
 where
  currentLine (x',_) _ = x' == x
  coords               = [ (x, y*2 - x) | y <- [0..x] ]

goOne :: Coord -> LengthMap -> LengthMap
goOne c@(x,y) = goTo (x+1,y-1) . goTo (x+1,y+1)
 where

  goTo c' m = alter (gogo v c') c' m
   where
    v | Just v' <- M.lookup c m = v'
      | otherwise               = weight c

  gogo v c' (Just w)
    | w > v + weight c' = Just w
  gogo v c' _           = Just (v + weight c')


main :: IO ()
main = print go
