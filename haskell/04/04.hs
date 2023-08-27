import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "04/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

type LinePair = (Int, Int, Int, Int)

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

aocParse :: Parser [LinePair]
aocParse = some parseLinePair <* eof
    where
    parseLinePair :: Parser LinePair
    parseLinePair = (, , ,) 
                  <$> nat <*> (lexeme "-" *> nat)
                  <*> (lexeme "," *> nat) <*> (lexeme "-" *> nat)
