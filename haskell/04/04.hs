import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "04/input.txt"
    let Right elves = parsed
    print $ partOne elves
    print $ partTwo elves

data Pair = Pair
    { _x1 :: Int
    , _x2 :: Int
    , _y1 :: Int
    , _y2 :: Int
    }

aocParse :: Parser [Pair]
aocParse = do
    some parsePair <* eof
    where
    parsePair :: Parser Pair
    parsePair = do
        Pair <$> nat 
             <*> (lexeme "-" *> nat)
             <*> (lexeme "," *> nat)
             <*> (lexeme "-" *> nat)

partOne :: [Pair] -> Int
partOne = sum . map contains
    where
    contains :: Pair -> Int
    contains (Pair a1 a2 b1 b2)
        | a1 <= b1 && b2 <= a2 = 1
        | b1 <= a1 && a2 <= b2 = 1
        | otherwise            = 0

partTwo :: [Pair] -> Int
partTwo = sum . map overlaps
    where
    overlaps :: Pair -> Int
    overlaps (Pair a1 a2 b1 b2)
        | a2 <= b2 && a2 >= b1 = 1
        | a2 >= b2 && a1 <= b2 = 1
        | otherwise            = 0
