import System.Environment
import qualified Data.Map as Map
import qualified Data.List as List

type Grid = Map.Map (Int, Int) Char

addToGrid :: Int -> Grid -> (Int, Char) -> Grid
addToGrid lsize grid (i, x) = Map.insert coords x grid
  where coords = divMod i lsize

zipWithIndex :: [Char] -> [(Int, Char)]
zipWithIndex l = zip [0..length l] l

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(xx, yy) | xx <- [x-1,x,x+1], yy <- [y-1,y,y+1], (xx,yy) /= (x,y)]

iterateG :: Grid -> Grid
iterateG grid = if newGrid == grid then grid else iterateG newGrid
  where newGrid = iterateOnce grid

iterateOnce :: Grid -> Grid
iterateOnce grid = Map.mapWithKey (flip' grid) grid

flip' :: Grid -> (Int, Int) -> Char -> Char
flip' _ _ '.' = '.'
flip' grid coords 'L' = if (nearbyOccupied grid coords) == 0 
  then '#' 
  else 'L'
flip' grid coords '#' = if (nearbyOccupied grid coords) >= 4
  then 'L'
  else '#'

nearbyOccupied :: Grid -> (Int, Int) -> Int
nearbyOccupied grid coords = length . filter (occupied grid) . adjacent $ coords
  where occupied grid coords = Map.member coords grid && (grid Map.! coords) == '#'

main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let lined = lines input
  print
    . length
    . filter (== '#')
    . Map.elems
    . iterateG
    . foldl (addToGrid (length (head lined))) (Map.empty :: Grid) 
    . zipWithIndex
    . concat
    $ lined
