import Data.List
import Data.Ord (clamp)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let moves = parse . words $ input
    print . partOne $ moves
    print . partTwo $ moves

type Coord = (Int, Int)
type Stepper = Coord -> Coord
type Move = (Stepper, Int)

parse :: [String] -> [Move]
parse (d : c : xs) =  (std d, read c) : parse xs
    where
        std :: String -> Stepper
        std "U" = (\(x, y) -> (x, y + 1))
        std "D" = (\(x, y) -> (x, y - 1))
        std "L" = (\(x, y) -> (x - 1, y))
        std "R" = (\(x, y) -> (x + 1, y))
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
doMove (_, 0) seen snake      = (seen, snake)
doMove (f, n) seen (s : nake) = doMove (f, n - 1) newSeen newSnake
    where
        newSeen = last newSnake : seen
        newSnake = slither $ (f s) : nake
doMove _ _ []                     = error "Snake mutilated"

slither :: [Coord] -> [Coord]
slither (hd : tl : r) = hd : slither (newtl : r)
    where newtl = follow tl hd
slither end           = end

follow :: Coord -> Coord -> Coord
follow (x, y) (hx, hy)
    | abs dx < 2 && abs dy < 2 = (x, y) -- Only move when necessary
    | otherwise                = (x + stp dx, y + stp dy)
    where
        (dx, dy) = (hx - x, hy - y)
        stp :: Int -> Int
        stp = clamp (-1, 1)
