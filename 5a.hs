import System.Environment
import Data.List (splitAt)

-- Accepts an initial range and narrows it down according to encoded instructions
codeRange :: (Char, Char) -> Int -> Int -> String -> Int
codeRange (_, _) acc _ [] = acc
codeRange (cl, ch) acc v (s:ss)
    | s == ch   = codeRange (cl, ch) (acc + v) (div v 2) ss
    | otherwise = codeRange (cl, ch) acc (div v 2) ss

row :: String -> Int
row = codeRange ('F', 'B') 0 64

col :: String -> Int
col = codeRange ('L', 'R') 0 4

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
