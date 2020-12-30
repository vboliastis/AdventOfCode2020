import System.Environment
import qualified Data.Map as Map
import qualified Data.List as List

type Grid = Map.Map (Int, Int) Char

addToGrid :: Int -> Grid -> (Int, Char) -> Grid
addToGrid lsize grid (i, x) = Map.insert coords x grid
  where coords = divMod i lsize

zipWithIndex :: [Char] -> [(Int, Char)]
zipWithIndex l = zip [0..length l] l

orientations :: [(Int, Int)]
orientations = [(x, y) | x <- [-1,0,1], y <- [-1,0,1], (x,y) /= (0,0)]

runThrough :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
runThrough (a, b) (x, y) = next : runThrough next (x, y)
  where next = (a + x, b + y)

searchThrough :: Grid -> [(Int, Int)] -> Bool
searchThrough grid (x:xs) = case Map.lookup x grid of
  Nothing -> False
  Just(c) -> if c == '.' 
    then searchThrough grid xs
    else c == '#'

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
flip' grid coords '#' = if (nearbyOccupied grid coords) >= 5
  then 'L'
  else '#'

nearbyOccupied :: Grid -> (Int, Int) -> Int
nearbyOccupied grid spot = length . filter (searchThrough grid) $ paths
  where paths = map (runThrough spot) orientations

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
