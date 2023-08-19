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

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space space1 empty empty

decimal :: Parser Int
decimal = lexeme L.decimal

type Arr2D a = Array (Int, Int) a

zipWithArr :: (a -> b -> c) -> Arr2D a -> Arr2D b -> Arr2D c
zipWithArr f a1 a2 = array bnds 
                   $ zip rang
                   $ fmap lf rang
    where
        bnds = bounds a1
        rang = range bnds
        lf = liftA2 f (a1 !) (a2 !)
