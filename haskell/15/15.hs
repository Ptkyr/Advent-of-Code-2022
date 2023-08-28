import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "15/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
--            print $ partTwo input

partOne :: Network -> Int
partOne (Network net x x') = length 
                           $ filter (inCoverage True net)
                           $ zip [x..x'] $ repeat 2000000

partTwo :: Network -> Int
partTwo (Network net _ _) = a * 4000000 + b
    where
    maxOut = 4000000
    search = [0..maxOut]
    (a, b) = head 
           . dropWhile (inCoverage False net) 
           $ [(i, j) | i <- search, j <- search]

type Coverage = Arr2D Bool

--coverage :: [Coord] -> [Sensor] -> Coverage
--coverage coords sensors = 
--    where
--    edge :: Coord -> Sensor -> Bool
--    edge c s = manhat c (_sensor s) == _range s


inCoverage :: Bool -> [Sensor] -> Coord -> Bool
inCoverage checkBeacon net coord = foldl' thing False net
    where 
    thing :: Bool -> Sensor -> Bool
    thing b sens = b || isCovered checkBeacon coord sens

isCovered :: Bool -> Coord -> Sensor -> Bool
isCovered checkBeacon c s = if checkBeacon 
                            then c /= _beacon s && inrange
                            else inrange
    where inrange = manhat c (_sensor s) <= _range s

manhat :: Coord -> Coord -> Int
manhat = phoenix (+) fst snd .: liftT2 (abs .: (-))

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
        sx <- lexeme "x=" *> lexeme int <* lexeme ","
        sy <- lexeme "y=" *> lexeme int <* lexeme ":"
        void $ lexeme "closest beacon is at"
        bx <- lexeme "x=" *> lexeme int <* lexeme ","
        by <- lexeme "y=" *> lexeme int
        pure $ Sensor (sx, sy) (bx, by) (manhat (sx, sy) (bx, by))
