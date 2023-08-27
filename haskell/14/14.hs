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
partTwo cave = leftTri + rightTri + dropSand p2End 1 cave
    where
    ((x1, _), (x2, y2)) = bounds cave
    leftTri  = nthTri $ y2 - 500 + x1 -- faster to compute
    rightTri = nthTri $ 500 + y2 - x2 --  trivially filled
    p2End :: Coord -> Cave -> Bool
    p2End cur cave' = cave'!cur

dropSand :: (Coord -> Cave -> Bool) -> Int -> Cave -> Int
dropSand endCnd i curCave = case dropUnit curCave of
    Just newCave -> dropSand endCnd (i + 1) newCave
    Nothing      -> i
    where
    dropUnit :: Cave -> Maybe Cave
    dropUnit = doFall (500, 0)
        where
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
    let xMin = sub1 $ fst $ minimumBy (phi compare fst) allRocks
    let xMax = add1 $ fst $ maximumBy (phi compare fst) allRocks
    let yMax = add1 $ snd $ maximumBy (phi compare snd) allRocks
    let grid = listArray ((xMin, 0), (xMax, yMax)) (repeat False)
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
