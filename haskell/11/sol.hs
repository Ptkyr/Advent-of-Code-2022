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
import Data.Either (fromRight)

main :: IO ()
main = do
    input <- readFile "11/input.txt"
    let parsed = runParser aocParse "what" $ pack input
    let Right monkeys = parsed
    print $ partOne monkeys

type ArrMonkey = Array Int Monke

partOne :: ArrMonkey -> Int
partOne ma = product 
           . take 2 
           . sortBy (flip compare) 
           . map ac 
           . elems 
           $ iterate doRound ma !! 20

doRound :: ArrMonkey -> ArrMonkey
doRound a = foldl monkeyDo a $ indices a

monkeyDo :: ArrMonkey -> Int -> ArrMonkey
monkeyDo a i = case a!i of
    (Monke _ [] _ _)       -> a
    (Monke c (x : xs) o t) -> monkeyDo (a // [mi, mo]) i
        where
        mi = (i, Monke (c + 1) xs o t)
        nw = div3 $ o x
        ni = t nw
        mm = a!ni
        mo = (ni, mm {is = is mm ++ [nw]})

div3 :: Int -> Int
div3 x = x `div` 3

type Parser = Parsec Void Text

data Monke = Monke
    { ac :: Int
    , is :: [Int]
    , op :: Int -> Int
    , ts :: Int -> Int
    }

instance Show Monke where
    show (Monke c s _ _) = show c
                           ++ ", "
                           ++ show s

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space space1 empty empty

decimal :: Parser Int
decimal = lexeme L.decimal

monkx :: Parser ()
monkx = do
    lexeme "Monkey"
    decimal
    void $ lexeme ":"

items :: Parser [Int]
items = do
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

oper :: Parser (Int -> Int)
oper = do
    lexeme "Operation: new = old"
    op <- lexeme asciiChar
    n <- lexeme $ some alphaNumChar
    let x = readMaybe n
    return (case x of
        Nothing  -> (\x -> x * x)
        Just num -> if op == '+' then (+) num 
                                 else (*) num)

test :: Parser (Int -> Int)
test = do
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

oneMonkey :: Parser Monke
oneMonkey = do
    monkx
    i <- items
    o <- oper
    t <- test
    return (Monke 0 i o t)

aocParse :: Parser ArrMonkey
aocParse = try $ do
    mlst <- some oneMonkey
    eof
    let bndslst = (0, length mlst - 1)
    return (array bndslst $ zip (range bndslst) $ mlst)
