import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "03/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

type Rucksack = [Int]

partOne :: [Rucksack] -> Int
partOne = sum . map rucks
    where 
    rucks :: Rucksack -> Int
    rucks s = common $ splitAt h s
        where
        h = length s `div` 2
        common :: (Rucksack, Rucksack) -> Int
        common ([], _)     = error "Unreachable"
        common (a : as, bs)
            | elem a bs = a
            | otherwise = common (as, bs)

partTwo :: [Rucksack] -> Int
partTwo (a : b : c : ss) = sack3 a b c + partTwo ss
    where
    sack3 :: Rucksack -> Rucksack -> Rucksack -> Int
    sack3 [] _ _               = error "Unreachable"
    sack3 (x : xs) y z
        | elem x y && elem x z = x
        | otherwise            = sack3 xs y z
partTwo _                      = 0

aocParse :: Parser [Rucksack]
aocParse = parseRuck `endBy` newline <* eof
    where
    parseRuck :: Parser Rucksack
    parseRuck = some $ priority <$> letterChar
    priority :: Char -> Int
    priority c
        | c >= 'A' && c <= 'Z' = c' - intA + 27
        | otherwise            = c' - inta + 1
        where
        c'   = fromEnum c 
        inta = fromEnum 'a' 
        intA = fromEnum 'A'
