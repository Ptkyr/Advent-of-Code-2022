import Utils
import Data.Array

main :: IO ()
main = do
    parsed <- parseInput aocParse "11/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

data Monkey = Monkey
    { activity :: Int
    , items    :: [Int]
    , _oper    :: Int -> Int
    , _tester  :: Int -> Int
    }

partOne :: Arr Monkey -> Int
partOne = monkeyBusiness 20 (flip div 3)

partTwo :: Arr Monkey -> Int
partTwo = monkeyBusiness 10000 (flip rem 9699690) -- To-do: not hardcoded

monkeyBusiness :: Int -> (Int -> Int) -> Arr Monkey -> Int
monkeyBusiness iters trunc am
    = product 
    . take 2 
    . sortBy (flip compare) 
    . map activity
    . elems 
    $ iterate (doRound trunc) am !! iters

doRound :: (Int -> Int) -> Arr Monkey -> Arr Monkey
doRound trunc am = foldl' (doInspection trunc) am $ indices am

doInspection :: (Int -> Int) -> Arr Monkey -> Int -> Arr Monkey
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

-- All parsing from here
parseItems :: Parser [Int]
parseItems = lexeme "Starting items:" *> nat `sepBy1` lexeme ","

parseOper :: Parser (Int -> Int)
parseOper = do
    operator <- lexeme "Operation: new = old" *> lexeme asciiChar
    operand  <- lexeme $ some alphaNumChar
    let mx = readMaybe operand
    pure $ case mx of
        Nothing  -> (\x -> x * x) -- "old", hardcoded and bad?
        Just num -> if operator == '+' 
                    then (+) num 
                    else (*) num

parseTester :: Parser (Int -> Int)
parseTester = makeTest <$> (lexeme "Test: divisible by"        *> nat)
                       <*> (lexeme "If true: throw to monkey"  *> nat)
                       <*> (lexeme "If false: throw to monkey" *> nat)
    where 
    makeTest :: Int -> Int -> Int -> Int -> Int
    makeTest m t f x
        | x `rem` m == 0 = t
        | otherwise      = f

oneMonkey :: Parser Monkey
oneMonkey = do
    void $ lexeme "Monkey" *> nat *> lexeme ":" 
    Monkey 0 <$> parseItems <*> parseOper <*> parseTester

aocParse :: Parser (Arr Monkey)
aocParse = listArr0 <$> some oneMonkey <* eof
