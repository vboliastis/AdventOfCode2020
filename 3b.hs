import System.Environment

instructions :: [(Int, Int)]
instructions = [(1,1),(3,1),(5,1),(7,1),(1,2)]

-- traverses a NxN grid according to insruction of the form (shift, step)
-- and returns # of trees encountered
traversePaths:: [[Char]] -> Int -> (Int, Int) -> Int
traversePaths paths rowSize (shift, step) = trees
    where (trees, _, _, _, _, _) = foldl slope (0, 0, rowSize, shift, step, 0) paths

-- evaluates a path row and updates current aggregated state; used as a reducer function
slope :: (Int, Int, Int, Int, Int, Int) -> [Char] -> (Int, Int, Int, Int, Int, Int)
slope (trees, pos, length, shift, step, counter) row
    | ((mod counter step) /= 0) = (trees, pos, length, shift, step, counter + 1)
    | ((row !! limpos) == '#')  = (trees + 1, limpos + shift, length, shift, step, counter + 1)
    | otherwise                 = (trees, limpos + shift, length, shift, step, counter + 1)
    where
        limpos = if pos >= length then (pos - length) else pos

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let paths = lines input
    let pathSize = length $ head paths
    print $ product $ map (traversePaths paths pathSize) instructions
