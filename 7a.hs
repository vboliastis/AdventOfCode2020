import System.Environment
import Data.List.Split (splitOn)
import Data.List (nub)
import Data.Map (Map, empty, fromList, unionWith, lookup)

type Content      = (Int, String)
type ContentGraph = Map String [String]

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
addToGraph acc (_, []) = acc
addToGraph acc (name, cs) = unionWith (++) acc invertedMap
    where invertedMap = fromList $ zip (map snd cs) $ replicate (length cs) [name]

traverseGraph :: String -> ContentGraph -> [String]
traverseGraph target graph = case Data.Map.lookup target graph of
    Just(contents) -> contents ++ concat (map (\c -> traverseGraph c graph) contents)
    Nothing        -> []

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print . length . nub . traverseGraph "shiny gold" . foldl addToGraph empty . map parseLine . lines $ input
