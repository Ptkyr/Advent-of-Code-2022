import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "01/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

aocParse :: Parser [Int]
aocParse = do
    map sum <$> oneElf `sepBy` newline <* eof
    where
    oneElf :: Parser [Int]
    oneElf = do
        (read <$> some digitChar) `endBy` newline

partOne :: [Int] -> Int
partOne = maximum

partTwo :: [Int] -> Int
partTwo = sum . take 3 . sortBy (flip compare)
