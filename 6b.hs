import System.Environment
import Data.List.Split
import Data.List (intersect)

answers :: String -> Int
answers = length . (foldl1 intersect) . (splitOn "\n")

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print $ (sum . (map answers) . (splitOn "\n\n")) input
