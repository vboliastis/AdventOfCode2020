import System.Environment
import Data.Either
import Data.List (sum)
import Data.Char (isDigit)
import Data.Map (Map, union, elems, empty, fromList)
import Data.Bits (setBit, clearBit)

type Mem = (Int, Int)

applyMask :: String -> Int -> [[Bool]]
applyMask mask n = combineBits mask $ toBits n 2

generate :: [[Bool]] -> [Int]
generate source = map (buildFromBits 1) combs
  where combs = cartesianProduct source

toBits :: Int -> Int -> [Bool]
toBits 0 _ = []
toBits n c = if m > 0 
  then True : toBits nn cc
  else False : toBits nn cc
  where 
    m  = mod n c
    nn = n - m
    cc = 2 * c

combineBits :: [Char] -> [Bool] -> [[Bool]]
combineBits [] _            = []
combineBits m []            = combineBits m $ repeat False
combineBits ('X':ms) (n:ns) = [False, True] : combineBits ms ns
combineBits (m:ms) (n:ns)   = [m == '1' || n] : combineBits ms ns

cartesianProduct :: [[Bool]] -> [[Bool]]
cartesianProduct = foldr (\xs as -> [x : a | x <- xs , a <- as]) [[]]

buildFromBits :: Int -> [Bool] -> Int
buildFromBits _ []        = 0
buildFromBits n (True:bs) = n + buildFromBits (2 * n) bs
buildFromBits n (_:bs)    = buildFromBits (2 * n) bs

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
run (db, mask) (Left (k, v))   = (union addition db, mask)
  where
    addresses = generate $ applyMask (reverse mask) k
    addition  = fromList (zip addresses (replicate (length addresses) v))

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
