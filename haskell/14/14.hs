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

partOne :: Cave -> Int
partOne = dropSand p1End 0
    where
    p1End :: Coord -> Cave -> Bool
    p1End (_, y) cave = y == ayMax cave - 1 -- fallen into abyss

partTwo :: Cave -> Int
partTwo cave = leftTri 
             + rightTri 
             + dropSand p2End 1 cave -- why 1? Idk
    where
    ((x, _), (x', y')) = bounds cave
    -- Compute the trivially filled spillover by the sides
    leftTri  = nthTri $ y' - 500 + x
    rightTri = nthTri $ y' - x' + 500
    p2End :: Coord -> Cave -> Bool
    p2End cur cave' = cave'!cur

dropSand :: (Coord -> Cave -> Bool) -> Int -> Cave -> Int
dropSand endCnd i curCave = case dropUnit curCave of
    Just newCave -> dropSand endCnd (i + 1) newCave
    Nothing      -> i
    where
    dropUnit :: Cave -> Maybe Cave
    dropUnit = doFall (500, 0)
    doFall :: Coord -> Cave -> Maybe Cave
    doFall cur@(x, y) cave
        | endCnd cur cave         = Nothing
        | not $ inRange bnds down = rest
        | not $ cave!down         = doFall down cave
        | not $ inRange bnds dlef = rest
        | not $ cave!dlef         = doFall dlef cave
        | not $ inRange bnds drit = rest 
        | not $ cave!drit         = doFall drit cave
        | otherwise               = rest
        where
        bnds = bounds cave
        y'   = y + 1
        down = (x, y')
        dlef = (x - 1, y')
        drit = (x + 1, y')
        rest = Just $ cave // [(cur, True)]

aocParse :: Parser Cave
aocParse = do
    allRocks <- concat <$> some parseRock <* eof
    let xRks = map fst allRocks
    let xMin = sub1 $ minimum xRks
    let xMax = add1 $ maximum xRks
    let yMax = add1 . maximum $ map snd allRocks
    let grid = listArray ((xMin, 0), (xMax, yMax)) $ repeat False
    pure $ grid // zip allRocks (repeat True)
    where
    parseRock :: Parser Rock
    parseRock = nub
              . concat 
              . mapAdjacent fillLine 
              <$> parseCoord `sepBy1` lexeme "->"
    parseCoord :: Parser Coord
    parseCoord = (, ) <$> (lexeme nat <* lexeme ",")
                      <*> lexeme nat
