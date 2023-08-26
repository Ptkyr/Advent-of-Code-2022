module Utils 
    ( module Utils
    , module Data.Array
    , module Data.Char
    , module Data.List
    , module Text.Megaparsec 
    , module Text.Megaparsec.Char
    , module Text.Megaparsec.Debug
    , readMaybe
    , void
    , module Data.Void
    , Text
    , pack
    , clamp
    ) where

import Text.Megaparsec hiding (parse) -- main module
import Text.Megaparsec.Char -- common combinators for character streams
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Read (readMaybe)
import Control.Monad (void)
import Data.Void
import Data.Text (Text, pack)
import Data.Char
import Data.List
import Data.Array
import Control.Applicative
import Data.Ord (clamp)
import Data.Either

-- Parser util
type Parser = Parsec Void Text

eatSome :: Parser ()
eatSome = L.space space1 empty empty

eatMany :: Parser ()
eatMany = L.space space empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme eatSome

symbol :: Text -> Parser Text
symbol = L.symbol eatSome

nat :: Parser Int
nat = lexeme L.decimal

int :: Parser Int
int = L.signed eatSome nat

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parseInput :: Parser a -> String -> IO (Either (ParseErrorBundle Text Void) a)
parseInput parser file = runParser parser file . pack <$> readFile file

-- Array util
type Arr a   = Array Int a
type Arr2D a = Array (Int, Int) a

-- Hopefully bounds a1 == bounds a2
zipWithArr2D :: (a -> b -> c) -> Arr2D a -> Arr2D b -> Arr2D c
zipWithArr2D f a1 a2 = listArray (bounds a1) $ zipWith f (elems a1) (elems a2)

-- Construct a 1-indexed array
listArr1 :: [a] -> Arr a
listArr1 arr = listArray (1, length arr) arr

-- Construct a 0-indexed array
listArr0 :: [a] -> Arr a
listArr0 arr = listArray (0, length arr - 1) arr

-- Construct a (1, 1)-indexed 2D array
listArr2D1 :: (a -> b) -> [[a]] -> Arr2D b
listArr2D1 f arr = listArray ((1, 1), (x, y)) 
                 $ concat $ map (map f) arr
    where
    x = length arr
    y = length $ head arr

-- Construct a (0, 0)-indexed 2D array
listArr2D0 :: (a -> b) -> [[a]] -> Arr2D b
listArr2D0 f arr = listArray ((0, 0), (x - 1, y - 1))
                 $ concat $ map (map f) arr
    where
    x = length arr
    y = length $ head arr
