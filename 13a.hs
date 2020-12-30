import System.Environment
import Data.List.Split
import Data.List (minimumBy)
import Data.Ord

parse :: [String] -> (Int, [Int])
parse [begin, rawTss] = (read begin, tss)
    where tss = map read . filter (/= "x") . splitOn (",") $ rawTss


arrivals :: (Int, [Int]) -> [(Int, Int)]
arrivals (_, [])        = []
arrivals (begin, (t:ts)) = (t, t - (mod begin t)) : arrivals (begin, ts)

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print 
        . (\(a, b) -> a * b)
        . minimumBy (comparing snd)
        . arrivals 
        . parse 
        . lines 
        $ input
