import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "04/input.txt"
    let Right elves = parsed
    print $ partOne elves
    print $ partTwo elves

type LinePair = (Int, Int, Int, Int)

aocParse :: Parser [LinePair]
aocParse = do
    some parseLinePair <* eof
    where
    parseLinePair :: Parser LinePair
    parseLinePair = do
        (, , ,) <$> nat <*> (lexeme "-" *> nat)
                <*> (lexeme "," *> nat) <*> (lexeme "-" *> nat)

partOne :: [LinePair] -> Int
partOne = sum . map contains
    where
    contains :: LinePair -> Int
    contains (a1, a2, b1, b2)
        | a1 <= b1 && b2 <= a2 = 1
        | b1 <= a1 && a2 <= b2 = 1
        | otherwise            = 0

partTwo :: [LinePair] -> Int
partTwo = sum . map overlaps
    where
    overlaps :: LinePair -> Int
    overlaps (a1, a2, b1, b2)
        | a2 <= b2 && a2 >= b1 = 1
        | a2 >= b2 && a1 <= b2 = 1
        | otherwise            = 0
