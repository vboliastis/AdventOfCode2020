import System.Environment
import Data.List.Split (splitOn)
import Data.String (lines, words)
import Data.Set (Set, fromList, union, notMember)

type Ticket = [Int]

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parseYt :: String -> Ticket
parseYt s = let [_, contents] = lines s in parseTicket contents

parseNt :: String -> [Ticket]
parseNt = map parseTicket . tail . lines

parseRule :: String -> Set Int
parseRule s = union (fromList [(read fa :: Int)..(read fb :: Int)]) (fromList [(read sa :: Int)..(read sb :: Int)])
    where
        (_, (_:rest))      = span (/= ':') s
        [first, _, second] = words rest
        [fa, fb]           = splitOn "-" first
        [sa, sb]           = splitOn "-" second

parseRules :: String -> Set Int
parseRules = foldl1 union . map parseRule . lines

errorRate :: Set Int -> Ticket -> Int
errorRate _ []         = 0
errorRate rules (x:xs) = let err = if notMember x rules then x else 0 in err + errorRate rules xs

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let [rulesRaw, ytRaw, ntRaw] = splitOn "\n\n" input
    let rules = parseRules rulesRaw
    let yt = parseYt ytRaw
    let nt = parseNt ntRaw
    print . sum . map (errorRate rules) $ nt
