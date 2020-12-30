import System.Environment

parseLine :: String -> (Char, Int)
parseLine (x:xs) = (x, read xs)

move :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
move (ship, (wx, wy)) ('N', dist) = (ship, (wx, wy + dist))
move (ship, (wx, wy)) ('S', dist) = (ship, (wx, wy - dist))
move (ship, (wx, wy)) ('E', dist) = (ship, (wx + dist, wy))
move (ship, (wx, wy)) ('W', dist) = (ship, (wx - dist, wy))
move (ship, wp) ('F', dist)       = (approach ship wp dist, wp)
move (ship, wp) (ori, deg)        = (ship, turn wp ori deg)

flipOri :: Char -> Char
flipOri 'L' = 'R'
flipOri _   = 'L'

turn :: (Int, Int) -> Char -> Int -> (Int, Int)
turn wp ori 270     = turn wp (flipOri ori) 90
turn (wx, wy) _ 180 = (-wx, -wy)
turn (wx, wy) 'R' _ = (wy, -wx)
turn (wx, wy) _ _   = (-wy, wx)

approach :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
approach (sx, sy) (wx, wy) dist = (sx + dist * wx, sy + dist * wy)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = (abs x) + (abs y)

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print 
        . manhattan
        . fst
        . foldl move ((0, 0), (10, 1))
        . map parseLine
        . lines 
        $ input