import System.Environment
import Data.Set (Set, empty, member, insert)
import Data.Maybe (Maybe)

-- Recursively traverses the list using a set as a caching mechanism
solve2s :: [Int] -> (Set Int) -> Int -> (Maybe Int)
solve2s [] _ _  = Nothing
solve2s (x:xs) cache target = 
    if (member x cache) then Just $ x * (target - x)
    else solve2s xs (insert (target - x) cache) target

-- For each sublist run a 2s problem solver at tail that searches for a sum so that sum + head = 2020
solve3s :: [Int] -> Int
solve3s (x:xs) = case solve2s xs Data.Set.empty (2020 - x) of
    Just partialResult -> x * partialResult
    Nothing -> solve3s xs

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let nums = map (\w -> read w :: Int) (lines input)
    print $ solve3s nums
