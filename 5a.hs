import System.Environment
import Data.List (splitAt)

-- Accepts an initial range and narrows it down according to encoded instructions
codeRange :: Char -> Int -> Int -> String -> Int
codeRange _ acc _ [] = acc
codeRange ch acc v (s:ss)
    | s == ch   = codeRange ch (acc + v) (div v 2) ss
    | otherwise = codeRange ch acc (div v 2) ss

row :: String -> Int
row = codeRange 'B' 0 64

col :: String -> Int
col = codeRange 'R' 0 4

seatId :: String -> Int
seatId s = let (r, c) = splitAt 7 s in (8 * (row r)) + col c

debug :: String -> (Int, Int)
debug s = (row r, col c)
    where (r, c) = splitAt 7 s

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let boardingPasses = lines input
    print $ maximum $ map seatId boardingPasses
