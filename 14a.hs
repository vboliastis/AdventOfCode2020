import System.Environment
import Data.Either
import Data.List (sum)
import Data.Char (isDigit)
import Data.Map (Map, insert, elems, empty)
import Data.Bits (setBit, clearBit)

type Mem = (Int, Int)

overwriteBit :: Int -> (Int, Char) -> Int
overwriteBit n (i, '1') = setBit n i
overwriteBit n (i, '0') = clearBit n i

applyMask :: String -> Int -> Int
applyMask mask x = foldl overwriteBit x bits
  where 
    lm   = length mask
    bits = (filter (\(_, x) -> x /= 'X') $ zip [lm-1,lm-2..0] mask) :: [(Int, Char)]


parse :: String -> Either Mem String
parse s
  | k == "mask " = Right $ drop 2 v
  | otherwise    = Left $ parseMem k v
  where
    (k, v) = span (/= '=') s

parseMem :: String -> String -> (Int, Int)
parseMem k v = (read (takeWhile isDigit (drop 4 k)), read $ (drop 2 v))

run :: (Map Int Int, String) -> Either Mem String -> (Map Int Int, String)
run (db, mask) (Right newmask) = (db, newmask)
run (db, mask) (Left (k, v))   = (insert k (applyMask mask v) db, mask)

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print
      . sum
      . elems
      . fst
      . foldl run (empty, "")
      . map parse
      . lines
      $ input
