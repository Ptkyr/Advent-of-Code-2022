import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "09/input.txt"
    let Right moves = parsed
    print $ partOne moves
    print $ partTwo moves

type Coord = (Int, Int)
type Stepper = Coord -> Coord
type Move = (Stepper, Int)

aocParse :: Parser [Move]
aocParse = do
    some parseMove <* eof
    where
    parseMove :: Parser Move
    parseMove = do
        stepper <- choice
            [ lexeme "U" *> (pure $ (\(x, y) -> (x, y + 1)))
            , lexeme "D" *> (pure $ (\(x, y) -> (x, y - 1)))
            , lexeme "L" *> (pure $ (\(x, y) -> (x - 1, y)))
            , lexeme "R" *> (pure $ (\(x, y) -> (x + 1, y)))
            ]
        steps <- nat
        pure $ (stepper, steps)

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
execute _ _ []                     = error "Snake mutilated"
execute (_, 0) seen snake      = (seen, snake)
execute (f, n) seen (s : nake) = execute (f, n - 1) newSeen newSnake
    where
    newSeen = last newSnake : seen
    newSnake = slither $ (f s) : nake

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
