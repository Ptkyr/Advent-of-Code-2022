import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "01/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

partOne :: [Int] -> Int
partOne = maximum

partTwo :: [Int] -> Int
partTwo = sum . take 3 . sortBy (flip compare)

aocParse :: Parser [Int]
aocParse = map sum <$> oneElf `sepBy1` newline <* eof
    where
    oneElf :: Parser [Int]
    oneElf = (read <$> some digitChar) `endBy` newline
