import System.Environment
import Data.Maybe (fromJust)

partA = 1212510616

findSum :: Int -> [Int] -> [Int]
findSum target xs = case search target xs of
    []     -> findSum target $ tail xs
    result -> result

search :: Int -> [Int] -> [Int]
search target (x:xs)
    | diff == 0 = [target]
    | diff < 0  = []
    | otherwise = case search diff xs of
        [] -> []
        result -> (x:result)
    where diff = target - x

sumEdges :: [Int] -> Int
sumEdges l = (minimum l) + (maximum l)

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    -- replace the number here with your part A solution
    print 
        . sumEdges 
        . findSum partA 
        . takeWhile (/= partA) 
        . map (read :: String -> Int) 
        . lines 
        $ input
