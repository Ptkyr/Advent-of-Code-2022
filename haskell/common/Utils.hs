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

zipWithArr :: (a -> b -> c) -> Arr2D a -> Arr2D b -> Arr2D c
zipWithArr f a1 a2 = array bnds 
                   $ zip rang
                   $ fmap lf rang
    where
    bnds = bounds a1
    rang = range bnds
    lf = liftA2 f (a1 !) (a2 !)
