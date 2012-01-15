import qualified Data.Map as M
import Data.Map (elems, Map, alter, filterWithKey, fromList)

triangle :: [[Int]]
triangle =
  [                             [75]
  ,                           [95, 64]
  ,                         [17, 47, 82]
  ,                       [18, 35, 87, 10]
  ,                     [20, 04, 82, 47, 65]
  ,                   [19, 01, 23, 75, 03, 34]
  ,                 [88, 02, 77, 73, 07, 63, 67]
  ,               [99, 65, 04, 28, 06, 16, 70, 92]
  ,             [41, 41, 26, 56, 83, 40, 80, 70, 33]
  ,           [41, 48, 72, 33, 47, 32, 37, 16, 94, 29]
  ,         [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14]
  ,       [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57]
  ,     [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48]
  ,   [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31]
  , [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23] ]

-- the position in the triangle is as follows
--
-- x:    y...
-- -----------------------------------
-- 0:                0 
-- 1:             -1   1
-- 2:          -2    0    2
-- 3:       -3    -1   1     3
-- 4:    -4    -2    0    2     4
--
-- etc..

type Coord = (Int,Int)

-- the "weight" is simply the value at the given position
weight :: Coord -> Int
weight (x,y) = (triangle !! x) !! ((y+x)`div`2)

done :: Coord -> Bool
done (x,_) = x == length triangle - 1

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
