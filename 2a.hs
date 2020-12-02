import System.Environment
import Data.List.Split

count :: Int -> Bool -> Int
count x valid 
    | valid = x + 1 
    | otherwise = x

valid :: (Int, Int, Char, [Char]) -> Bool
valid (min, max, _, []) = min <= 0 && max >= 0
valid (min, max, t, (s:ss))
    | (t == s) && max == 0 = False
    | (t == s)             = valid ((min - 1), (max - 1), t, ss)
    | otherwise            = valid (min, max, t, ss)

parse :: [Char] -> (Int, Int, Char, [Char])
parse s = (ia, ib, hc, d)
    where 
        [a, b, c, d] = filter (not . null) $ (splitOneOf ":- ") s
        (ia, ib) = (read a :: Int, read b :: Int)
        [hc] = c

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let passwords = map parse (lines input)
    print $ foldl count 0 $ map valid passwords
