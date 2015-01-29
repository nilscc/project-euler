import Data.List
import Control.Monad

import Data.P022

value :: String -> Int
value = sum . map charVal

charVal :: Char -> Int
charVal c = maybe (error "Nothing!") fst . find ((c ==) . snd) $ zip [1..] ['A'..'Z']

main :: IO ()
main = do
  (_,r) <- foldM go (1,0) (sort names)
  print r

go :: (Int,Int) -> String -> IO (Int,Int)
go (pos,s) name = do
  -- debugging, hehe
  putStrLn $ "Name " ++ name ++ " at position " ++ show pos ++ ", value " ++ show (value name * pos)
  return (pos+1, value name * pos + s)
