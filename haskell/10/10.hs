import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "10/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            putStr $ partTwo input

type CPU = (Int, Int)
type Inst = (Int -> Int, Int)

aocParse :: Parser [Inst]
aocParse = many parseInst <* eof
    where
    parseInst :: Parser Inst
    parseInst = choice
        [ lexeme "noop" *> (pure (id, 1))
        , lexeme "addx" *> ((, 2) <$> ((+) <$> int))
        ]

execute :: CPU -> [Inst] -> [CPU]
execute _ []                 = []
execute cpu@(x, c) ((act, cyc) : is)
    | cyc == 1    = cpu : execute (act x, c + 1) is
    | otherwise   = cpu : execute (x, c + 1) ((act, cyc - 1) : is)

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
