import Utils
import Data.Array

main :: IO ()
main = do
    parsed <- parseInput aocParse "15/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

type Network = [Sensor]

data Sensor = Sensor
    { _sensor :: Coord
    , _beacon :: Coord
    , _range  :: Int
    } deriving (Show)

partOne :: Network -> Int
partOne network = length 
                $ filter (inCoverage network)
                $ zip [x..x'] $ repeat 2000000
    where
    sens = map (fst . _sensor) network
    xmin = minimum sens
    xmax = maximum sens
    rmax = maximum $ map _range network
    x    = xmin - rmax
    x'   = xmax + rmax

partTwo :: Network -> Int
partTwo network = a * 4000000 + b
    where
    maxOut = 4000000
    (a, b) = head 
           . dropWhile (inCoverage network) 
           . filter (inRange ((0, 0), (maxOut, maxOut)))
           $ blindSpots network
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
    foldCover res (Sensor s b r) = res || isCovered
        where isCovered = coord /= b && manhattan coord s <= r

aocParse :: Parser Network
aocParse = some parseSensor <* eof
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
