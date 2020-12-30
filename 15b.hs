import System.Environment
import Data.List.Split
import Data.Map (Map, insert, fromList, lookup)

parse :: String -> [Int]
parse = map read . splitOn ","

iter :: [Int] -> [Int]
iter l = l ++ (iter2 (ll+1) (fromList (zip (init l) [1..ll-1])) (last l))
    where
        ll = length l

iter2 :: Int -> Map Int Int -> Int -> [Int]
iter2 30000001  _ _ = []
iter2 c db n = case Data.Map.lookup n db of
    Nothing -> 0 : iter2 (c+1) (insert n (c-1) db) 0
    Just(f) -> res : iter2 (c+1) (insert n (c-1) db) res
        where res = c - f - 1

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print . last . iter . parse $ input
