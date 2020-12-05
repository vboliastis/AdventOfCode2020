import System.Environment
import Data.Map (Map, fromList, size, notMember, assocs)
import Data.Char (isDigit, isAlphaNum)
import Data.Set (Set, fromDistinctAscList, member)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)

type Passport = Map String String

-- Parses a multiline passport representation
parsePassport :: String -> Passport
parsePassport = fromList . (map parseKV) . words

-- Parses a key:value passport part
parseKV :: String -> (String, String)
parseKV s = let (a, b) = splitAt 3 s in (a, tail b)

-- verifies that a passport includes either all fields or all except cid
validPassportType :: Passport -> Bool
validPassportType p = let s = size p 
    in s == 8 || (s == 7 && (notMember "cid" p))

-- type and value verification
validPassport :: Passport -> Int
validPassport p = fromEnum $ validPassportType p && (all validKV (assocs p))

-- takes a string and verifies that it's a number within range
numInRange :: String -> (Int, Int) -> Bool
numInRange s (minv, maxv) = all isDigit s 
    && let n = read s :: Int in n >= minv && n <= maxv

eyeColors :: Set String
eyeColors = fromDistinctAscList ["amb", "blu", "brn", "grn", "gry", "hzl", "oth"]

validKV :: (String, String) -> Bool
validKV ("byr", v) = numInRange v (1920, 2002)
validKV ("iyr", v) = numInRange v (2010, 2020)
validKV ("eyr", v) = numInRange v (2020, 2030)
validKV ("hgt", v)
    | isSuffixOf "cm" v = numInRange ((init . init) v) (150, 193)
    | isSuffixOf "in" v = numInRange ((init . init) v) (59, 76)
    | otherwise         = False
validKV ("hcl", v) = (head v) == '#' 
    && let tv = tail v in (length tv) == 6 && all isAlphaNum tv
validKV ("ecl", v) = member v eyeColors
validKV ("pid", v) = (length v) == 9 && all isDigit v
validKV ("cid", _) = True

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print $ sum $ map (validPassport . parsePassport) $ splitOn "\n\n" input
