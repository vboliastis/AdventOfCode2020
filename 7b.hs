import System.Environment
import Data.List.Split (splitOn)
import Data.Map (Map, empty, fromList, insert, (!))

type Content      = (Int, String)
type ContentGraph = Map String [Content]

parseContents :: [Content] -> [String] -> [Content]
parseContents acc [] = acc
parseContents acc ("no":ws) = []
parseContents acc (w1:w2:w3:_:ws) = parseContents ((a, b):acc) ws
    where
        a = read w1 :: Int
        b = unwords [w2, w3]

parseLine :: String -> (String, [Content])
parseLine s = (sa, contents)
    where
        [sa, sb] = splitOn " bags contain " s
        contents = parseContents [] . words $ sb

addToGraph :: ContentGraph -> (String, [Content]) -> ContentGraph
addToGraph acc (k, v) = insert k v acc

traverseGraph :: String -> ContentGraph -> Int
traverseGraph target graph = sum . map (\c -> let fc = fst c in fc + (fc * (traverseGraph (snd c) graph))) $ graph ! target

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print . traverseGraph "shiny gold" . foldl addToGraph empty . map parseLine . lines $ input
