import Utils

main :: IO ()
main = do
    input <- readFile "09/input.txt"
    let moves = parse . words $ input
    print . partOne $ moves
    print . partTwo $ moves

type Coord = (Int, Int)
type Stepper = Coord -> Coord
type Move = (Stepper, Int)

parse :: [String] -> [Move]
parse (d : c : xs) =  (toStepper d, read c) : parse xs
    where
        toStepper :: String -> Stepper
        toStepper "U" = (\(x, y) -> (x, y + 1))
        toStepper "D" = (\(x, y) -> (x, y - 1))
        toStepper "L" = (\(x, y) -> (x - 1, y))
        toStepper "R" = (\(x, y) -> (x + 1, y))
        toStepper _   = error "Unreachable"
parse _            = []

partOne :: [Move] -> Int
partOne = visited 2

partTwo :: [Move] -> Int
partTwo = visited 10

visited :: Int -> [Move] -> Int
visited snakeLen = length
                 . group
                 . sort
                 . tailPositions [origin] (replicate snakeLen origin)
    where origin = (0, 0)

tailPositions :: [Coord] -> [Coord] -> [Move] -> [Coord]
tailPositions seen _ []           = seen
tailPositions seen snake (m : ms) = tailPositions newSeen newSnake ms
    where (newSeen, newSnake) = execute m seen snake

execute :: Move -> [Coord] -> [Coord] -> ([Coord], [Coord])
execute (_, 0) seen snake      = (seen, snake)
execute (f, n) seen (s : nake) = execute (f, n - 1) newSeen newSnake
    where
        newSeen = last newSnake : seen
        newSnake = slither $ (f s) : nake
execute _ _ []                     = error "Snake mutilated"

slither :: [Coord] -> [Coord]
slither (hd : tl : r) = hd : slither (follow tl hd : r)
slither end           = end

follow :: Coord -> Coord -> Coord
follow (x, y) (hx, hy)
    | abs dx < 2 && abs dy < 2 = (x, y) -- Only move when necessary
    | otherwise                = (x + step dx, y + step dy)
    where
        (dx, dy) = (hx - x, hy - y)
        step :: Int -> Int
        step = clamp (-1, 1)
