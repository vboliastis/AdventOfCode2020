import System.Environment
import Data.List.Split
import Data.List (union)

answers :: String -> Int
answers = length . (foldl1 union) . (splitOn "\n")

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print $ (sum . (map answers) . (splitOn "\n\n")) input
