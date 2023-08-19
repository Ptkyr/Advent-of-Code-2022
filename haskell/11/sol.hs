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
monkeyBusiness iters trunc ma 
    = product 
    . take 2 
    . sortBy (flip compare) 
    . map activity
    . elems 
    $ iterate (doRound trunc) ma !! iters

doRound :: (Int -> Int) -> ArrMonkey -> ArrMonkey
doRound trunc a = foldl (doInspection trunc) a $ indices a

doInspection :: (Int -> Int) -> ArrMonkey -> Int -> ArrMonkey
doInspection trunc a i = case a!i of
    (Monkey _ [] _ _)         -> a
    (Monkey act (x : xs) o t) -> doInspection trunc (a // [cur, new]) i
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

parseItems :: Parser [Int]
parseItems = do
    lexeme "Starting items:"
    decimal `sepBy` lexeme ","

parseOper :: Parser (Int -> Int)
parseOper = do
    lexeme "Operation: new = old"
    operator <- lexeme asciiChar
    operand  <- lexeme $ some alphaNumChar
    let x = readMaybe operand
    pure $ case x of
        Nothing  -> (\x -> x * x) -- "old", maybe hardcoded?
        Just num -> if operator == '+' 
                    then (+) num 
                    else (*) num

parseTester :: Parser (Int -> Int)
parseTester = do
    modulus <- lexeme "Test: divisible by"        *> decimal
    onTrue  <- lexeme "If true: throw to monkey"  *> decimal
    onFalse <- lexeme "If false: throw to monkey" *> decimal
    pure $ makeTest modulus onTrue onFalse
    where 
        makeTest :: Int -> Int -> Int -> Int -> Int
        makeTest m t f x
            | x `rem` m == 0 = t
            | otherwise      = f

oneMonkey :: Parser Monkey
oneMonkey = do
    lexeme "Monkey" *> decimal *> lexeme ":"
    Monkey 0 <$> parseItems <*> parseOper <*> parseTester

aocParse :: Parser ArrMonkey
aocParse = do
    mlst <- some oneMonkey <* eof
    let bndslst = (0, length mlst - 1)
    pure $ array bndslst $ zip (range bndslst) $ mlst
