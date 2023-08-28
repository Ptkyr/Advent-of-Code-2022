module Utils 
    ( module Utils
    , module Data.Void
    , module Data.Array
    , module Data.Char
    , module Data.List
    , module Text.Megaparsec 
    , module Text.Megaparsec.Char
    , module Text.Megaparsec.Debug
    , module Data.Ord
    , module Data.List.HT
    , module Data.Function
    , readMaybe
    , void
    , Text
    , pack
    , amap
    ) where

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Read (readMaybe)
import Control.Monad (void)
import Data.Void
import Data.Text (Text, pack)
import Data.Char
import Data.List
import Data.Array
import Control.Applicative hiding (some)
import Data.Ord
import Data.Either
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.List.HT (mapAdjacent)
import Data.Function
import GHC.Arr (amap)

-- Typedefs
type Coord = (Int, Int)

-- Misc util
add1 :: Int -> Int
add1 = flip (+) 1

sub1 :: Int -> Int
sub1 = flip (+) (-1)

clamp2D :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int)
clamp2D ((x, y), (x', y')) (a, b) = (clamp (x, x') a, clamp (y, y') b)

-- Will drop the last element if odd
toPairs :: [a] -> [(a, a)]
toPairs (x : y : zs) = (x, y) : toPairs zs
toPairs []           = []
toPairs (z : zs)     = []

-- Generates line segment between two endpoints
fillLine :: Coord -> Coord -> [Coord]
fillLine (x, y) (x', y')
    | x == x' = zip (repeat x) [min y y'..max y y']
    | y == y' = zip [min x x'..max x x'] $ repeat y
    | otherwise = error "To-do"

nthTri :: Int -> Int
nthTri n = (n * (n + 1)) `div` 2

-- Combinators
phi :: (b -> y -> c) -> (a -> b) -> (x -> y) -> a -> x -> c
phi bin un un' a1 a2 = bin (un a1) (un' a2)

b1 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
b1 un bin x y = un $ bin x y

liftT1 :: (a -> b) -> (a, a) -> (b, b)
liftT1 f (x, y) = (f x, f y)

liftT2 :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
liftT2 f a b = (on f fst a b, on f snd a b)

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

word :: Parser String
word = some letterChar

lexword :: Parser String
lexword = lexeme $ some letterChar

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
listArr2D1 :: [[a]] -> Arr2D a
listArr2D1 arr = listArray ((1, 1), (x, y)) 
                 $ concat arr
    where
    x = length arr
    y = length $ head arr

-- Construct a (0, 0)-indexed 2D array
listArr2D0 :: [[a]] -> Arr2D a
listArr2D0 arr = listArray ((0, 0), (x - 1, y - 1))
                 $ concat arr
    where
    x = length arr
    y = length $ head arr

-- WARNING: partial function
indexByValue :: (Ix i, Eq e) => e -> Array i e -> i
indexByValue val = fst 
                 . head 
                 . dropWhile (\a -> snd a /= val)
                 . assocs

ayMax :: Arr2D e -> Int
ayMax = snd . snd . bounds

axMax :: Arr2D e -> Int
axMax = fst . snd . bounds

-- Dijkstra's
type Graph  = Arr2D Node
type Node   = Char
type DijkPQ = PQ.PQueue Int Coord

data Dijkstra = Dijkstra
    { _start :: Coord
    , _end   :: Coord 
    , _graph :: Graph
    , _costs :: Arr2D Int
    } deriving (Show)

neighbours :: Graph -> Coord -> (Node -> Node -> Bool) -> [Coord]
neighbours graph v@(vx, vy) fltr 
    = filter liftFilter
    $ map (clamp2D (bounds graph)) nbrslist
    where 
    nbrslist = [(vx - 1, vy), (vx + 1, vy),
                (vx, vy - 1), (vx, vy + 1)]
    liftFilter :: (Int, Int) -> Bool
    liftFilter nbr = fltr vAt nbrAt && nbr /= v -- fix clamping
        where
        nbrAt = graph!nbr
        vAt   = graph!v

dijkstra :: (Coord -> Bool) -> (Node -> Node -> Bool) -> Dijkstra -> Int
dijkstra endCond adjCond info@(Dijkstra s e _ c) 
    = dijk' info {_costs = c // [(s, 0)]} 
    $ PQ.singleton 0 s
    where
    dijk' :: Dijkstra -> DijkPQ -> Int
    dijk' d@(Dijkstra _ _ graph costs) pq = case PQ.minViewWithKey pq of
        Nothing            -> error "Unreachable"
        Just ((cost, coord), pq')
            | endCond coord -> costs!coord
            | otherwise     -> dijk' d {_costs = recurCosts} recurPQ
            where
            (recurCosts, recurPQ) = updateCosts (neighbours graph coord adjCond) costs pq'
            newCost               = cost + 1
            updateCosts :: [Coord] -> Arr2D Int -> DijkPQ -> (Arr2D Int, DijkPQ)
            updateCosts [] oldC oldPQ = (oldC, oldPQ)
            updateCosts (n : nbrs) oldC oldPQ
                | newCost < costs!n   = updateCosts nbrs (oldC // [(n, newCost)]) 
                                      $ PQ.insert newCost n oldPQ
                | otherwise           = updateCosts nbrs oldC oldPQ
