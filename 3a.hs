import System.Environment

-- traverses a NxN grid on a hardcoded (1,3) instruction
slope :: (Int, Int, Int) -> [Char] -> (Int, Int, Int)
slope (trees, pos, length) row
    | ((row !! limpos) == '#') = (trees + 1, limpos + 3, length)
    | otherwise                = (trees, limpos + 3, length)
    where
        limpos = if pos >= length then (pos - length) else pos

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let paths = lines input
    let (trees, _, _) = foldl slope (0, 0, length (head paths)) paths
    print trees
