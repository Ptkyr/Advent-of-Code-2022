import Data.List
import Data.Ord (clamp)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let moves = parse . words $ input
    print . partOne $ moves
    print . partTwo $ moves

data Dir = U | D | L | R
    deriving (Show)

data Move = Move
    { dir :: Dir
    , cnt :: Int
    } deriving (Show)

type Coord = (Int, Int)

parse :: [String] -> [Move]
parse (d : c : xs) = Move (std d) (read c) : parse xs
    where
        std :: String -> Dir
        std "U" = U
        std "D" = D
        std "L" = L
        std "R" = R
        std _   = error "Unreachable"
parse _            = []

partOne :: [Move] -> Int
partOne = visited 2

partTwo :: [Move] -> Int
partTwo = visited 10

visited :: Int -> [Move] -> Int
visited snakeLen = length
                 . group
                 . sort
                 . execute [origin] (replicate snakeLen origin)
    where origin = (0, 0)

execute :: [Coord] -> [Coord] -> [Move] -> [Coord]
execute seen _ []           = seen
execute seen snake (m : ms) = execute newSeen newSnake ms
    where (newSeen, newSnake) = doMove m seen snake

doMove :: Move -> [Coord] -> [Coord] -> ([Coord], [Coord])
doMove (Move _ 0) seen snake      = (seen, snake)
doMove (Move d n) seen (s : nake) = doMove (Move d $ n - 1) newSeen newSnake
    where
        newSeen = last newSnake : seen
        newSnake = slither $ (step d s) : nake
doMove _ _ []                     = error "Snake mutilated"

slither :: [Coord] -> [Coord]
slither (hd : tl : r)
        | tl == newtl = hd : tl : r -- No propagation when motionless
        | otherwise   = hd : slither (newtl : r)
    where newtl = follow tl hd
slither ftl           = ftl

follow :: Coord -> Coord -> Coord
follow (x, y) (hx, hy)
    | abs dx < 2 && abs dy < 2 = (x, y) -- Only move when necessary
    | otherwise                = (x + stp dx, y + stp dy)
    where
        (dx, dy) = (hx - x, hy - y)
        stp :: Int -> Int
        stp = clamp (-1, 1)

step :: Dir -> Coord -> Coord
step U (x, y) = (x, y + 1)
step D (x, y) = (x, y - 1)
step L (x, y) = (x - 1, y)
step R (x, y) = (x + 1, y)
