import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "14/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

type Rock = [Coord]
type Cave = Arr2D Bool

sandSource :: Coord
sandSource = (500, 0)

partOne :: Cave -> Int
partOne = dropSand p1End 0
    where
    p1End :: Coord -> Cave -> Bool
    p1End (_, y) cave = y == ayMax cave - 1

partTwo :: Cave -> Int
partTwo = dropSand p2End 1
    where
    p2End :: Coord -> Cave -> Bool
    p2End c@(x, y) cave =
        c == sandSource
        && cave!down
        && cave!dlef
        && cave!drit
        where
        y'   = y + 1
        down = (x, y')
        dlef = (x - 1, y')
        drit = (x + 1, y')

dropSand :: (Coord -> Cave -> Bool) -> Int -> Cave -> Int
dropSand endCnd i curCave = case dropUnit curCave of
    Just newCave -> dropSand endCnd (i + 1) newCave
    Nothing      -> i
    where
    dropUnit :: Cave -> Maybe Cave
    dropUnit = doFall sandSource
        where
        doFall :: Coord -> Cave -> Maybe Cave
        doFall cur@(x, y) cave
            | endCnd cur cave = Nothing
            | not $ cave!down = doFall down cave
            | not $ cave!dlef = doFall dlef cave
            | not $ cave!drit = doFall drit cave
            | otherwise       = Just $ cave // [(cur, True)]
            where
            y'   = y + 1
            down = (x, y')
            dlef = (x - 1, y')
            drit = (x + 1, y')

aocParse :: Parser Cave
aocParse = do
    allRocks <- concat <$> some parseRock <* eof
    let yMax = snd $ maximumBy (\x -> \y -> compare (snd x) (snd y)) allRocks
    let y'   = yMax + 2
    let xMin = fst sandSource - y'
    let xMax = fst sandSource + y'
    let grid = listArray ((xMin, 0), (xMax, y')) (repeat False)
    let bttm = zip [xMin..xMax] (repeat y')
    pure $ grid // zip (allRocks ++ bttm) (repeat True)
    where
    parseRock :: Parser Rock
    parseRock = nub
              . concat 
              . mapAdjacent fillLine 
              <$> parseCoord `sepBy1` lexeme "->"
    parseCoord :: Parser Coord
    parseCoord = (, ) <$> (lexeme nat <* lexeme ",")
                      <*> lexeme nat
