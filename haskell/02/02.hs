import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "02/input.txt"
    let Right rps = parsed
    print $ partOne rps
    print $ partTwo rps

data RPS = RPS
    { _foe :: Int
    , _me  :: Int
    }

aocParse :: Parser [RPS]
aocParse = do
    some oneRPS <* eof
    where
    oneRPS :: Parser RPS
    oneRPS = do 
        f <- lexeme upperChar
        m <- lexeme upperChar
        pure $ RPS (fromEnum f - fromEnum 'A') (fromEnum m - fromEnum 'X')

partOne :: [RPS] -> Int
partOne = sum . map toScore
    where
    toScore :: RPS -> Int
    toScore (RPS f m) = (m' - f) `mod` 3 * 3 + m'
        where m' = m + 1

partTwo :: [RPS] -> Int
partTwo = sum . map toScore
    where
    toScore :: RPS -> Int
    toScore (RPS f m) = (f + m - 1) `mod` 3 + m * 3 + 1
