import System.Environment
import Data.List.Split (splitOn)
import Data.String (lines, words)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

type Ticket = [Int]

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parseYt :: String -> Ticket
parseYt s = let [_, contents] = lines s in parseTicket contents

parseNt :: String -> [Ticket]
parseNt = map parseTicket . tail . lines

parseRule :: String -> (String, (Set.Set Int))
parseRule s = (name, Set.union (Set.fromList [(read fa :: Int)..(read fb :: Int)]) (Set.fromList [(read sa :: Int)..(read sb :: Int)]))
    where
        (name, (_:rest)) = span (/= ':') s
        [first, _, second] = words rest
        [fa, fb] = splitOn "-" first
        [sa, sb] = splitOn "-" second

parseRules :: String -> (Map.Map String (Set.Set Int), Set.Set Int)
parseRules s = (Map.fromList rules, unified)
    where
        rules   = map parseRule . lines $ s
        unified = foldl1 Set.union . map snd $ rules

valid :: Set.Set Int -> Ticket -> Bool
valid rules ts = all (\t -> Set.member t rules) ts

narrowDown :: Map.Map String (Set.Set Int) -> [[String]] -> Ticket -> [[String]]
narrowDown _ [] []             = []
narrowDown rules (c:cs) (t:ts) = discardCandidateRules rules c t : narrowDown rules cs ts

discardCandidateRules :: Map.Map String (Set.Set Int) -> [String] -> Int -> [String]
discardCandidateRules rules cnds n = filter (\cnd -> Set.member n (rules Map.! cnd)) cnds

continuousStrictOut :: [(Int, [String])] -> [(Int, [String])]
continuousStrictOut s = if newS == s then s else continuousStrictOut newS
    where newS = strictOut s

strictOut :: [(Int, [String])] -> [(Int, [String])]
strictOut [] = []
strictOut (s:ss)
    | (length cnds) == 1 = s : next
    | otherwise          = (i, (cnds List.\\ singles)) : next
    where
        (i, cnds) = s
        next      = strictOut ss
        singles   = concat $ filter ((== 1) . length ) . map snd $ ss

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let [rulesRaw, ytRaw, ntRaw] = splitOn "\n\n" input
    let (discrete, unified) = parseRules rulesRaw
    let yt = parseYt ytRaw
    let nt = parseNt ntRaw
    let correctNt = filter (valid unified) $ nt
    print 
        . product
        . map (\(i, _) -> yt !! i)
        . filter (List.isPrefixOf "departure" . head . snd)
        . continuousStrictOut 
        . reverse 
        . List.sortOn (length . snd)
        . zip [0..length yt]
        . (foldl (narrowDown discrete) (replicate (length yt) (Map.keys discrete))) 
        $ correctNt
