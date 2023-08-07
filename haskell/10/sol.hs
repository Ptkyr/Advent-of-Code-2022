main :: IO ()
main = do
    input <- readFile "input.txt"
    let asm = parse . words $ input
    print . partOne $ asm
    print . partTwo $ asm

type Act = Int -> Int
type Inst = (Act, Int)
type CPU = (Int, Int)

parse :: [String] -> [Inst]
parse ("addx" : n : xs) = ((+) $ read n, 2) : parse xs
parse ("noop" : xs)     = (id, 1) : parse xs
parse _                 = []

execute :: CPU -> [Inst] -> [CPU]
execute _ []                 = []
execute (x, c) ((i, l) : is) = (x, c) : execute newCPU newIns
    where 
        modify = l == 1
        newCPU = if modify then (i x, c + 1) 
                           else (x, c + 1)
        newIns = if modify then is           
                           else ((i, l - 1) : is)

partOne :: [Inst] -> Int
partOne = sum . map report . execute (1, 1)
    where 
        report :: CPU -> Int
        report (x, c)
            | c `rem` 40 == 20 = x * c
            | otherwise        = 0

partTwo :: [Inst] -> String
partTwo = concat . map drawer . execute (1, 1)
    where
        drawer :: CPU -> String
        drawer (x, c) = char ++ eol
            where 
                crtPos = (c - 1) `rem` 40
                eol  = if crtPos == 39 then "\n"
                                       else ""
                char = if abs (x - crtPos) < 2 then "#"
                                               else "."
