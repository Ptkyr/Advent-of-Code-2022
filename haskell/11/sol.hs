import Text.Megaparsec -- main module
import Text.Megaparsec.Char -- common combinators for character streams
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Read (readMaybe)
import Data.Void
import Data.Text (Text, pack)
import Data.Char

main :: IO ()
main = do
    input <- readFile "11/input.txt"
    let monkey = runParser aocParse "what" $ pack input
    print monkey

type Parser = Parsec Void Text

data Monke = Monke
    { ix :: Int
    , is :: [Int]
    , op :: Int -> Int
    , ts :: Int -> Int
    , ac :: Int
    }

instance Show Monke where
    show (Monke x s _ _ c) = "\n"
                           ++ show x 
                           ++ ", "
                           ++ show s
                           ++ ", "
                           ++ show c

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space space1 empty empty

decimal :: Parser Int
decimal = lexeme L.decimal

index :: Parser Int
index = do
    lexeme "Monkey"
    i <- decimal
    lexeme ":"
    return i

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
        Just num -> if op == '+' then (+) num else (*) num)

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
            | x `div` m == 0 = t
            | otherwise      = f

oneMonkey :: Parser Monke
oneMonkey = do
    m <- index
    i <- items
    o <- oper
    t <- test
    return (Monke m i o t 0)

aocParse :: Parser [Monke]
aocParse = try $ do
    mlst <- some oneMonkey
    eof
    return mlst
