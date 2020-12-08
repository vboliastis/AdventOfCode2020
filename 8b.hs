import System.Environment
import Data.Map (Map, fromList, lookup, delete, keys)
import Data.Maybe (fromJust)

type Cmd = (Int, Int) -> (Int, Int)

makeCmd :: Int -> Int -> Cmd
makeCmd x y = \(a, b) -> (a + x, b + y)

nop :: Cmd
nop = makeCmd 0 1

jmp :: Int -> Cmd
jmp n = makeCmd 0 n

acc :: Int -> Cmd
acc n = makeCmd n 1

parseInt :: String -> Int
parseInt n@('-':_) = read n
parseInt (_:n)     = read n

parseCmd :: String -> (Cmd, Cmd)
parseCmd s = case splitAt 4 s of
    ("nop ", n) -> (nop, jmp (parseInt n))
    ("acc ", n) -> let nn = parseInt n in (acc nn, acc nn)
    ("jmp ", n) -> (jmp (parseInt n), nop)

parseCmds :: [String] -> Map Int (Cmd, Cmd)
parseCmds ss = fromList $ zip [0..(length ss)] $ map parseCmd ss

execute :: (Int, Int) -> Bool -> Int -> Map Int (Cmd, Cmd) -> Maybe Int
execute state@(acc, ins) flipped finish cmds = case Data.Map.lookup ins cmds of
    Nothing   -> Nothing
    Just(cmd) -> if ins == finish 
        then 
            Just(fst ((fst cmd) state))
        else 
            case execute ((fst cmd) state) flipped finish rest of
                Just(result) -> Just(result)
                Nothing -> if flipped 
                    then 
                        Nothing
                    else
                        execute ((snd cmd) state) True finish rest
        where rest = delete ins cmds

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let cmds = parseCmds . lines $ input
    print . fromJust . (execute (0, 0) False ((length cmds) - 1)) $ cmds
