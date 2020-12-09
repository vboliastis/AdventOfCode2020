import System.Environment
import Data.Set (Set, fromList, notMember, delete, insert)

iter :: (Set Int, [Int], [Int]) -> Int
iter (sset, pre, (p:post)) = if nomatch p sset pre
    then p
    else iter ((insert p (delete hpre sset)), tpre ++ [p], post)
    where (hpre:tpre) = pre

nomatch :: Int -> Set Int -> [Int] -> Bool
nomatch _ _ [] = True
nomatch target cache (l:ls) = (notMember (target - l) cache)
    && nomatch target cache ls

preprocess :: [Int] -> (Set Int, [Int], [Int])
preprocess l = (fromList l, pre, post)
    where (pre, post)= splitAt 25 l

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print . iter . preprocess . map (read :: String -> Int) . lines $ input
