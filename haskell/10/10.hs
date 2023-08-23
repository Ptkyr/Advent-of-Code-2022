import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "10/input.txt"
    let Right asm = parsed
    print $ partOne asm
    putStr $ partTwo asm

type CPU = (Int, Int)

data Inst = Inst
    { _cycles :: Int
    , _action :: Int -> Int
    }

aocParse :: Parser [Inst]
aocParse = many parseInst <* eof
    where
    parseInst :: Parser Inst
    parseInst = choice
        [ lexeme "noop" *> (pure $ Inst 1 id)
        , lexeme "addx" *> (Inst 2 <$> ((+) <$> int))
        ]

execute :: CPU -> [Inst] -> [CPU]
execute _ []                 = []
execute cpu@(x, c) (Inst cyc act : is)
    | cyc == 1    = cpu : execute (act x, c') is
    | otherwise   = cpu : execute (x, c') (Inst (cyc - 1) act : is)
    where c' = c + 1

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
