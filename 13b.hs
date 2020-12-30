import System.Environment
import Data.List.Split
import Data.List (minimumBy, maximumBy, find)
import Data.Ord
import Data.Maybe (fromJust)

preprocess :: [String] -> [(Integer, Integer)]
preprocess [_, rawTss] = map (\(i, n) -> (fromIntegral i, read n :: Integer)) zipped
    where
        elements = splitOn (",") $ rawTss
        zipped = filter (\(_, b) -> b /= "x") $ zip [0,-1..0 - (length elements)] $ elements

beizut :: Integer -> Integer -> (Integer, Integer)
beizut a b = beizutIter (a, b) (1, 0) (0, 1)

beizutIter :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
beizutIter (_, 0) (s0, _) (t0, _)  = (s0, t0)
beizutIter (r0, r) (s0, s) (t0, t) = beizutIter (r, r0 - (q * r)) (s, s0 - (q * s)) (t, t0 - (q * t))
    where q = div r0 r

solve2 :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solve2 (rema, qa) (remb, qb) = (mod (res + prod) prod, prod)
    where
        (coefa, coefb) = beizut qa qb
        res = (rema * coefb * qb) + (remb * coefa * qa)
        prod = qa * qb

normalize :: [(Integer, Integer)] -> [(Integer, Integer)]
normalize l = map (\(a, b) -> (mod a b, b)) l

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print 
        . fst 
        . foldl1 solve2 
        . normalize 
        . preprocess 
        . lines 
        $ input
