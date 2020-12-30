import System.Environment

parseLine :: String -> (Char, Int)
parseLine (x:xs) = (x, read xs)

move :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
move ((x, y), dir) ('N', dist) = ((x, y + dist), dir)
move ((x, y), dir) ('S', dist) = ((x, y - dist), dir)
move ((x, y), dir) ('E', dist) = ((x + dist, y), dir)
move ((x, y), dir) ('W', dist) = ((x - dist, y), dir)
move ((x, y), dir@(dx, dy)) ('F', dist) = ((x + (dx * dist), y + (dy * dist)), dir)
move (pos, dir) (ori, deg) = (pos, turn ori deg dir)

flipOri :: Char -> Char
flipOri 'L' = 'R'
flipOri _   = 'L'

turn :: Char -> Int -> (Int, Int) -> (Int, Int)
turn _ 180 (x, y) = (-x, -y)
turn ori 270 dir  = turn (flipOri ori) 90 dir
turn 'R' _ (x, y) = (y, -x)
turn _ _ (x, y)   = (-y, x)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = (abs x) + (abs y)

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print 
        . manhattan
        . fst
        . foldl move ((0, 0), (1, 0))
        . map parseLine
        . lines 
        $ input
