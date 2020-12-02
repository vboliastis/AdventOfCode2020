import System.Environment
import Data.List.Split

count :: Int -> Bool -> Int
count x valid 
    | valid = x + 1 
    | otherwise = x

valid :: (Int, Int, Char, [Char]) -> Bool
valid (i1, i2, target, s) = (s !! (i1-1) == target) /= (s !! (i2-1) == target)

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
