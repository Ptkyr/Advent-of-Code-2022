import Text.Megaparsec -- main module
import Text.Megaparsec.Char -- common combinators for character streams
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Read (readMaybe)
import Control.Monad (void)
import Data.Void
import Data.Text (Text, pack)
import Data.Char
import Data.Array
import Data.List

main :: IO ()
main = do
    input <- readFile "11/input.txt"
    let parsed = runParser aocParse "what" $ pack input
    let Right monkeys = parsed
    print $ partOne monkeys
    print $ partTwo monkeys

data Monkey = Monkey
    { activity :: Int
    , items    :: [Int]
    , oper     :: Int -> Int
    , test     :: Int -> Int
    }

type ArrMonkey = Array Int Monkey

partOne :: ArrMonkey -> Int
partOne = solve 20 (flip div 3)

partTwo :: ArrMonkey -> Int
partTwo = solve 10000 (flip rem 9699690) -- ToDo: not hardcoded

solve :: Int -> (Int -> Int) -> ArrMonkey -> Int
solve iters trunc ma = product 
                     . take 2 
                     . sortBy (flip compare) 
                     . map activity
                     . elems 
                     $ iterate (doRound trunc) ma !! iters

doRound :: (Int -> Int) -> ArrMonkey -> ArrMonkey
doRound trunc a = foldl (monkeyDo trunc) a $ indices a

monkeyDo :: (Int -> Int) -> ArrMonkey -> Int -> ArrMonkey
monkeyDo trunc a i = case a!i of
    (Monkey _ [] _ _)       -> a
    (Monkey act (x : xs) o t) -> monkeyDo trunc (a // [cur, new]) i
        where
        cur      = (i, Monkey (act + 1) xs o t)
        newWorry = trunc $ o x
        targetIx = t newWorry
        target   = a!targetIx
        new      = (targetIx, target {items = items target
                                            ++ [newWorry]})

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space space1 empty empty

decimal :: Parser Int
decimal = lexeme L.decimal

monkx :: Parser ()
monkx = do
    lexeme "Monkey"
    decimal
    void $ lexeme ":"

parseItems :: Parser [Int]
parseItems = do
    lexeme "Starting items:"
    lst <- many lstitem
    end <- decimal
    return $ lst ++ [end]
    where
        lstitem :: Parser Int
        lstitem = try $ do
            i <- decimal 
            lexeme "," -- no comma -> backtrack for last item
            return i

parseOper :: Parser (Int -> Int)
parseOper = do
    lexeme "Operation: new = old"
    op <- lexeme asciiChar
    n <- lexeme $ some alphaNumChar
    let x = readMaybe n
    return (case x of
        Nothing  -> (\x -> x * x)
        Just num -> if op == '+' then (+) num 
                                 else (*) num)

parseTester :: Parser (Int -> Int)
parseTester = do
    lexeme "Test: divisible by"
    modulus <- decimal
    lexeme "If true: throw to monkey"
    onTrue  <- decimal
    lexeme "If false: throw to monkey"
    onFalse <- decimal
    return $ makeTest modulus onTrue onFalse
    where 
        makeTest :: Int -> Int -> Int -> Int -> Int
        makeTest m t f x
            | x `rem` m == 0 = t
            | otherwise      = f

oneMonkey :: Parser Monkey
oneMonkey = do
    monkx
    i <- parseItems
    o <- parseOper
    t <- parseTester
    return (Monkey 0 i o t)

aocParse :: Parser ArrMonkey
aocParse = do
    mlst <- some oneMonkey
    eof
    let bndslst = (0, length mlst - 1)
    return (array bndslst $ zip (range bndslst) $ mlst)
