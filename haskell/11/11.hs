import Utils

main :: IO ()
main = do
    input <- readFile "11/input.txt"
    let parsed = parse aocParse "" $ pack input
    let Right monkeys = parsed
    print $ partOne monkeys
    print $ partTwo monkeys

data Monkey = Monkey
    { activity :: Int
    , items    :: [Int]
    , oper     :: Int -> Int
    , tester   :: Int -> Int
    }

type ArrMonkey = Array Int Monkey

partOne :: ArrMonkey -> Int
partOne = monkeyBusiness 20 (flip div 3)

partTwo :: ArrMonkey -> Int
partTwo = monkeyBusiness 10000 (flip rem 9699690) -- To-do: not hardcoded

monkeyBusiness :: Int -> (Int -> Int) -> ArrMonkey -> Int
monkeyBusiness iters trunc am
    = product 
    . take 2 
    . sortBy (flip compare) 
    . map activity
    . elems 
    $ iterate (doRound trunc) am !! iters

doRound :: (Int -> Int) -> ArrMonkey -> ArrMonkey
doRound trunc am = foldl' (doInspection trunc) am $ indices am

doInspection :: (Int -> Int) -> ArrMonkey -> Int -> ArrMonkey
doInspection trunc am i = case am!i of
    (Monkey _ [] _ _)         -> am
    (Monkey act (x : xs) o t) -> doInspection trunc (am // [cur, new]) i
        where
        cur      = (i, Monkey (act + 1) xs o t)
        newWorry = trunc $ o x
        targetIx = t newWorry
        target   = am!targetIx
        new      = (targetIx, target {items = items target
                                            ++ [newWorry]})

parseItems :: Parser [Int]
parseItems = do
    lexeme "Starting items:" *> decimal `sepBy` lexeme ","

parseOper :: Parser (Int -> Int)
parseOper = do
    operator <- lexeme "Operation: new = old" *> lexeme asciiChar
    operand  <- lexeme $ some alphaNumChar
    let mx = readMaybe operand
    pure $ case mx of
        Nothing  -> (\x -> x * x) -- "old", maybe hardcoded and bad?
        Just num -> if operator == '+' 
                    then (+) num 
                    else (*) num

parseTester :: Parser (Int -> Int)
parseTester = do
    makeTest <$> (lexeme "Test: divisible by"        *> decimal)
             <*> (lexeme "If true: throw to monkey"  *> decimal)
             <*> (lexeme "If false: throw to monkey" *> decimal)
    where 
    makeTest :: Int -> Int -> Int -> Int -> Int
    makeTest m t f x
        | x `rem` m == 0 = t
        | otherwise      = f

oneMonkey :: Parser Monkey
oneMonkey = do
    void $ lexeme "Monkey" *> decimal *> lexeme ":" 
    Monkey 0 <$> parseItems <*> parseOper <*> parseTester

aocParse :: Parser ArrMonkey
aocParse = do
    mlst <- some oneMonkey <* eof
    let bndslst = (0, length mlst - 1)
    pure $ array bndslst $ zip (range bndslst) $ mlst