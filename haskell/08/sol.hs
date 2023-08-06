import Data.Array
import Data.Char
import Control.Applicative
import Data.Ord (clamp)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let forest = parseToArr . words $ input
    print . partOne $ forest
    print . partTwo $ forest

type Arr2DInt = Array (Int, Int) Int
type Arr2D a = Array (Int, Int) a

zipWithArr :: (a -> a -> b) -> Arr2D a -> Arr2D a -> Arr2D b
zipWithArr f a1 a2 = array bnds 
                   $ zip rngs
                   $ fmap lf rngs
    where
        bnds = bounds a1
        rngs = range bnds
        lf = liftA2 f (a1 !) (a2 !)

parseToArr :: [String] -> Arr2DInt
parseToArr s = array bnds 
             $ zip rngs
             $ concat . map (map digitToInt) $ s
    where
        bnds = ((1, 1), (x, y))
        rngs = range bnds
        x = length s
        y = length . head $ s

partOne :: Arr2DInt -> Int
partOne a = sum . foldr (zipWithArr (ior)) u $ [l, d, r]
    where
        ior :: Int -> Int -> Int
        ior x y = clamp (0, 1) $ x + y
        u = visU a
        l = visL a
        d = visD a
        r = visR a

partTwo :: Arr2DInt -> Int
partTwo a = maximum . foldr (zipWithArr (*)) u $ [l, d, r]
    where
        u = viewU a
        l = viewL a
        d = viewD a
        r = viewR a

visDir :: (Int -> Int) -> (Int -> Int) 
         -> Arr2DInt -> Arr2DInt
visDir fx fy forest 
    = array bnds
    $ [((i, j), visComp (forest!(i, j)) (fx i) (fy j))
    | i <- [x..x']
    , j <- [y..y']]
    where
        bnds = bounds forest
        ((x, y), (x', y')) = bnds
        -- visComp; visible from (u, v) to the edge, step with fx/fy
        visComp :: Int -> Int -> Int -> Int
        visComp t u v
            | not . inRange bnds $ (u, v) = 1
            | t <= forest!(u, v)          = 0
            | otherwise                   = 1 * visComp t nx ny
            where (nx, ny) = (fx u, fy v)

viewDist :: (Int -> Int) -> (Int -> Int) 
         -> Arr2DInt -> Arr2DInt
viewDist fx fy forest 
    = array bnds
    $ [((i, j), vCount (forest!(i, j)) (fx i) (fy j))
    | i <- [x..x']
    , j <- [y..y']]
    where
        bnds = bounds forest
        ((x, y), (x', y')) = bnds
        -- vCount; # visible from (u, v) to the edge, step with fx/fy
        vCount :: Int -> Int -> Int -> Int
        vCount t u v
            | not . inRange bnds $ (u, v) = 0
            | t <= forest!(u, v)          = 1
            | otherwise                   = 1 + vCount t nx ny
            where (nx, ny) = (fx u, fy v)

visU :: Arr2DInt -> Arr2DInt
visU = visDir (+ (-1)) id

visL :: Arr2DInt -> Arr2DInt
visL = visDir id (+ (-1))

visD :: Arr2DInt -> Arr2DInt
visD = visDir (+ 1) id

visR :: Arr2DInt -> Arr2DInt
visR = visDir id (+ 1)

viewU :: Arr2DInt -> Arr2DInt
viewU = viewDist (+ (-1)) id

viewL :: Arr2DInt -> Arr2DInt
viewL = viewDist id (+ (-1))

viewD :: Arr2DInt -> Arr2DInt
viewD = viewDist (+ 1) id

viewR :: Arr2DInt -> Arr2DInt
viewR = viewDist id (+ 1)
