import System.Environment
import Data.List (sort)

diffs :: [Int] -> [Int]
diffs (x1:x2:[]) = [x2-x1]
diffs (x1:x2:xs) = (x2-x1) : (diffs (x2:xs))

splitDiff :: [Int] -> [[Int]]
splitDiff l = case span (/= 3) l of
    (a, []) -> [a]
    ([], b) -> splitDiff $ drop b
    (a, b)  -> a : (splitDiff $ drop b)
    where drop = dropWhile (== 3)
    

appendEdges :: [Int] -> [Int]
appendEdges l = (0:l) ++ [(last l) + 3]

combs :: [Int] -> Int
combs []            = 0
combs (_:[])        = 1
combs (x1:x2:x3:xs) = (combs (x2:x3:xs)) + a + b
    where
        a = if (x1 + x2) > 2 then 0 else combs ((x1+x2):x3:xs)
        b = if (x1 + x2 + x3) > 3 then 0 else combs ((x1+x2+x3):xs)
combs (x1:x2:xs)    = (combs (x2:xs)) + if (x1+x2) > 2 then 0 else (combs ((x1+x2):xs))

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print 
        . product
        . map combs
        . filter ((>1) . length)
        . splitDiff
        . diffs 
        . appendEdges 
        . sort 
        . map (read :: String -> Int) 
        . lines 
        $ input
