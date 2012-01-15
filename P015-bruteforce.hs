{-# LANGUAGE NamedFieldPuns #-}

import Data.List

data Coord = Coord   { x :: Int, y :: Int } deriving (Eq, Show)
type Path  = [Coord]
data Map   = Map     { sizeX :: Int, sizeY :: Int, path :: Path }

-- the two map sizes we'll play with
map2x2 :: Map
map2x2 = Map { sizeX = 2, sizeY = 2, path = [] }

map20x20 :: Map
map20x20 = Map { sizeX = 20, sizeY = 20, path = [] }

-- starting with a map at the top position
start :: Map -> Map
start m = m { path = [Coord 0 0] }

-- check if we're at the bottom right position
done :: Map -> Bool
done Map{ sizeX, sizeY, path }
  | null path              = False
  | Coord x y <- last path = x == sizeX && y == sizeY 

-- visualizing :)
drawMap :: Map -> String
drawMap Map{ sizeX, sizeY, path } =
  intercalate "\n" $
    map drawLine [0..sizeY]
 where
  drawLine    y = intersperse ' ' $ map (drawCoord `flip` y) [0..sizeX]
  drawCoord x y
    | Coord x y `elem` path = '+'
    | otherwise             = '·'

possibleMoves :: Map -> [Coord]
possibleMoves Map{ sizeX, sizeY, path } =
  concatMap toValidCoord 
    [ (x+1,y  )
    , (x  ,y+1) ]
 where
  Coord x y | null path = Coord 0 0
            | otherwise = last path

  toValidCoord (x',y')
    | x' <= sizeX && x' >= 0
    , y' <= sizeY && y' >= 0
    , not (Coord x' y' `elem` path)
    = [ Coord x' y' ]
    | otherwise
    = [ ]

addCoord :: Map -> Coord -> Map
addCoord m c =
  m { path = path m ++ [c] }

go :: Map -> [Map]
go m
  | done m    = [m]
  | otherwise = concatMap go $ goOne m

goOne :: Map -> [Map]
goOne m = map (addCoord m) (possibleMoves m)

main :: IO ()
main = print . length $ go (start map20x20)
