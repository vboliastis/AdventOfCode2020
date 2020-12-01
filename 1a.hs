import System.Environment
import Data.Set (Set, empty, member, insert)

-- Recursively traverses the list using a set as a caching mechanism
solve :: [Int] -> (Set Int) -> Int
solve (x:xs) cache = 
    if (member x cache) then x * (2020 - x)
    else solve xs (insert (2020 - x) cache)


main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let nums = map (\w -> read w :: Int) (lines input)
    print $ solve nums Data.Set.empty
