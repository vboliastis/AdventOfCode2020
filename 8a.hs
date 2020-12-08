import System.Environment
import Data.Map (Map, fromList, lookup, delete)

type Cmd = (Int, Int) -> (Int, Int)

makeCmd :: Int -> Int -> Cmd
makeCmd x y = \(a, b) -> (a + x, b + y)

parseInt :: String -> Int
parseInt n@('-':_) = read n
parseInt (_:n)     = read n

parseCmd :: String -> Cmd
parseCmd s = case splitAt 4 s of
    ("nop ", _) -> makeCmd 0 1
    ("acc ", n) -> makeCmd (parseInt n) 1
    ("jmp ", n) -> makeCmd 0 (parseInt n)

parseCmds :: [String] -> Map Int Cmd
parseCmds ss = fromList $ zip [0..(length ss)] $ map parseCmd ss

execute :: (Int, Int) -> Map Int Cmd -> Int
execute state@(acc, ins) cmds = case Data.Map.lookup ins cmds of
    Nothing   -> acc
    Just(cmd) -> execute (cmd state) (delete ins cmds)

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    print . (execute (0, 0)) . parseCmds . lines $ input
