import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "08/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

partOne :: Arr2D Int -> Int
partOne a = sum $ foldr (zipWithArr (ior)) u [l, d, r]
    where
        ior :: Int -> Int -> Int
        ior x y = clamp (0, 1) $ x + y
        u = visDir (+ (-1)) id a
        l = visDir id (+ (-1)) a
        d = visDir (+ 1) id    a
        r = visDir id (+ 1)    a

partTwo :: Arr2D Int -> Int
partTwo a = maximum $ foldr (zipWithArr (*)) u [l, d, r]
    where
        u = viewDist (+ (-1)) id a
        l = viewDist id (+ (-1)) a
        d = viewDist (+ 1) id    a
        r = viewDist id (+ 1)    a

valueAt :: Int -> Int -> (Int -> Int -> Int)
        -> (Int -> Int) -> (Int -> Int) -> Arr2D Int -> Arr2D Int
valueAt edge halt recur fx fy forest 
    = array bnds
    $ [((i, j), wave (forest!(i, j)) (fx i) (fy j))
    | i <- [x..x']
    , j <- [y..y']]
    where
    bnds@((x, y), (x', y')) = bounds forest
    -- wave; go from (u, v) to the edge, stepping with fx/fy
    wave :: Int -> Int -> Int -> Int
    wave t u v
        | not $ inRange bnds (u, v) = edge
        | t <= forest!(u, v)          = halt
        | otherwise                   = recur 1 $ wave t nx ny
        where (nx, ny) = (fx u, fy v)

visDir :: (Int -> Int) -> (Int -> Int) -> Arr2D Int -> Arr2D Int
visDir = valueAt 1 0 (*) 

viewDist :: (Int -> Int) -> (Int -> Int) -> Arr2D Int -> Arr2D Int
viewDist = valueAt 0 1 (+) 

aocParse :: Parser (Arr2D Int)
aocParse = do
    arr <- some digitChar `endBy` newline <* eof
    let x = length arr
    let y = length $ head arr
    pure $ listArray ((1, 1), (x, y)) $ concat $ map (map digitToInt) arr
