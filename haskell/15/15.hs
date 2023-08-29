import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "15/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

data Network = Network
    { _sensors :: [Sensor]
    , _xmin    :: Int
    , _xmax    :: Int
    } deriving (Show)

data Sensor = Sensor
    { _sensor :: Coord
    , _beacon :: Coord
    , _range  :: Int
    } deriving (Show)

partOne :: Network -> Int
partOne (Network net x x') = length 
                           $ filter (inCoverage net)
                           $ zip [x..x'] $ repeat 2000000

partTwo :: Network -> Int
partTwo (Network net _ _) = a * 4000000 + b
    where
    maxOut = 4000000
    (a, b) = head 
           . dropWhile (inCoverage net) 
           . filter (inRange ((0, 0), (maxOut, maxOut)))
           $ blindSpots net
    blindSpots :: [Sensor] -> [Coord]
    blindSpots = concat . map boundary 
    boundary :: Sensor -> [Coord]
    boundary (Sensor (xs, ys) _ r) 
        = concat 
        $ mapAdjacent fillLine 
        [upper, right, lower, left, upper]
        where
        upper = (xs, ys - r - 1)
        right = (xs + r + 1, ys)
        lower = (xs, ys + r + 1)
        left  = (xs - r - 1, ys)

inCoverage :: [Sensor] -> Coord -> Bool
inCoverage net coord = foldl' foldCover False net
    where 
    foldCover :: Bool -> Sensor -> Bool
    foldCover bl (Sensor s b r) = bl || isCovered
        where
        isCovered = coord /= b && manhattan coord s <= r

aocParse :: Parser Network
aocParse = do
    network <- some parseSensor <* eof
    let sensors = map (fst . _sensor) network
    let beacons = map (fst . _beacon) network
    let xmin = on min minimum sensors beacons
    let xmax = on max maximum sensors beacons
    let rmax = maximum $ map _range network
    pure $ Network network (xmin - rmax) (xmax + rmax)
    where
    parseSensor :: Parser Sensor
    parseSensor = do
        void $ lexeme "Sensor at"
        sx <- lexeme "x=" *> int <* lexeme ","
        sy <- lexeme "y=" *> int
        void $ lexeme ": closest beacon is at"
        bx <- lexeme "x=" *> int <* lexeme ","
        by <- lexeme "y=" *> int
        pure $ Sensor (sx, sy) (bx, by) (manhattan (sx, sy) (bx, by))
