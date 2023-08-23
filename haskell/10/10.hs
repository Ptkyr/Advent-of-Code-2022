import Utils

main :: IO ()
main = do
    parsed <- runParser aocParse "" . pack <$> readFile "10/input.txt"
    let Right asm = parsed
    print $ partOne asm
    putStr $ partTwo asm

type CPU = (Int, Int)

data Inst = Inst
    { _action :: Int -> Int
    , _cycles :: Int
    }

aocParse :: Parser [Inst]
aocParse = many parseInst <* eof
    where
    parseInst :: Parser Inst
    parseInst = noop <|> addx
        where
        noop = do 
            void $ lexeme "noop"
            pure $ Inst id 1
        addx = do
            void $ lexeme "addx"
            n <- int
            pure $ Inst ((+) n) 2

execute :: CPU -> [Inst] -> [CPU]
execute _ []                 = []
execute (x, c) (Inst i l : is) = (x, c) : execute newCPU newIns
    where 
        modify = l == 1
        newCPU = if modify then (i x, c + 1) 
                           else (x, c + 1)
        newIns = if modify then is           
                           else (Inst i $ l - 1) : is

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
        drawer (x, c) = disply ++ endLn
            where 
                crtPos = (c - 1) `rem` 40
                endLn  = if crtPos == 39 then "\n"
                                         else ""
                disply = if abs (x - crtPos) < 2 then "#"
                                                 else "."
