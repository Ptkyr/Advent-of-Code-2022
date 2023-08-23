import Utils

main :: IO ()
main = do
    parsed <- runParser aocParse "" . pack <$> readFile "02/input.txt"
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
    toScore (RPS f m) = 3 * ((m' - f) `mod` 3) + m'
        where m' = m + 1

partTwo :: [RPS] -> Int
partTwo = sum . map toScore
    where
    toScore :: RPS -> Int
    toScore (RPS f m) = 3 * m + (f + m - 1) `mod` 3 + 1
