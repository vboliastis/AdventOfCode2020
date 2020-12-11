import System.Environment
import Data.List (sort)

diffs :: [Int] -> [Int]
diffs (x1:x2:[]) = [x2-x1]
diffs (x1:x2:xs) = (x2-x1) : (diffs (x2:xs))

countA :: (Int, Int) -> [Int] -> Int
countA (s1, s3) [] = s1 * s3
countA (s1, s3) (1:xs) = countA (s1 + 1, s3) xs
countA (s1, s3) (3:xs) = countA (s1, s3 + 1) xs
countA acc (_:xs) = countA acc xs

appendEdges :: [Int] -> [Int]
appendEdges l = (0:l) ++ [(last l) + 3]

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print 
        . countA (0, 0) 
        . diffs 
        . appendEdges 
        . sort 
        . map (read :: String -> Int) 
        . lines 
        $ input
