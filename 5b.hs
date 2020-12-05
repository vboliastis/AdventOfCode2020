import System.Environment
import Data.List (splitAt, sort)

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

-- Cuts first and final 8 seats
centeredSeats :: [Int] -> [Int]
centeredSeats  = (take 1008) . (drop 8) . sort

missingSeat :: [Int] -> Int
missingSeat (s1:s2:[]) = s1 + 1
missingSeat (s1:s2:ss) = if s2 == (s1 + 2) then s1 + 1 else missingSeat (s2:ss)

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let boardingPasses = lines input
    print $ (missingSeat . centeredSeats . map seatId) boardingPasses
