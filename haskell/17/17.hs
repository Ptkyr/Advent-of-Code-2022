import Utils
import Data.Array
import Debug.Trace

main :: IO ()
main = do
    parsed <- parseInput aocParse "17/ex.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            --putStr . showCave $ tetris input

type Cave = Arr2D Bool

partOne :: Tetris -> Int
partOne = tetris 1

showCave :: Cave -> String
showCave cave = showHelp 1 $ elems cave
    where
    showHelp :: Int -> [Bool] -> String
    showHelp _ []         = []
    showHelp cnt (b : bs) = display ++ endline ++ showHelp (cnt + 1) bs
        where
        display = if b then ['#'] else ['.']
        endline = if cnt `mod` 7 == 0 then ['\n'] else []

data Rock = Rock
    { _shape  :: [Coord] -- track positions
    , _rockht :: Int -- just for how much to pad when placing
    , _drops  :: Int
    }

placeRock :: Rock -> Cave -> Cave
placeRock (Rock _ h _) c = listArray (origin, (newBound, 6))
                       $ blankRows h' ++ elems c
    where 
    (_, (x, _)) = bounds c
    h'          = h + 3
    oldCave     = elems c
    newBound    = if oldCave == []
                  then x + h' - 1
                  else x + h'

data Tetris = Tetris
    { _jets   :: [Jet]
    , _rocks  :: [Rock]
    , _cave   :: Cave
    , _clears :: Int
    , _place  :: Int
    }

-- Call this with tetris (result of aocParse)
--   which is the parsed input, rockOrder, an initial 0x0 cave
--   matrix, 2022 for the count, 0 as initial height
tetris :: Int -> Tetris -> Int
tetris rocksLeft t@(Tetris 
                    (jet : js) 
                    (cur@(Rock shape rh drops) : rs) 
                    cave clears place)
    | rocksLeft == 0 
    = trace ("ending\n" ++ show clears) axMax cave + clears + 1
    | place < 3 || canDrop -- lower the tile
    = trace ("dropping\n" ++ show shape) tetris rocksLeft (Tetris js (downCur : rs) 
                        cave clears place')
    | otherwise
    = trace ("resting\n" ++ showCave tncCave) tetris rL' (Tetris js rs tncCave
                  clears' 0)
    where
    rL'     = rocksLeft - 1
    place'  = place + 1
    transed = map jet shape
    jetted  = if validSpot transed cave
              then transed
              else shape
    downed  = map (\(a, b) -> (a + 1, b)) jetted
    canDrop = validSpot downed cave
    downCur = Rock downed rh $ drops + 1
    
    deltaH  = rh + 3 - drops
    hChange = deltaH /= 0
    padding = replicate (7 * deltaH) False
    
    padCave = trace (show deltaH) listArray (origin, (axMax cave + deltaH, 6)) 
            $ padding ++ elems cave

    newCave = padCave // (zip jetted $ repeat True)
  
    restTop = if drops - (rh + 3) > 0
              then drops - (rh + 3)
              else 0
    trunRow = filledRow newCave [restTop..restTop + rh - 1]
    clears' = clears + axMax newCave - trunRow -- no change if no clear
    cleared = clears' /= clears
    tncCave = listArray (origin, (trunRow, 6))
            $ elems newCave

tetris _ _       = error "Shut up GHC you're unexhaustive"

filledRow :: Cave -> [Int] -> Int
filledRow cave cands
    | cands == []       = axMax cave -- x adds 0 to clears'
    | cand > x          = x
    | fullRow cave cand = cand
    | otherwise         = filledRow cave $ tail cands
    where 
    cand = head cands
    x = axMax cave

calcHeight :: Int -> Cave -> Int
calcHeight cur cave = axMax cave - emptyRows cave
    where
    (_, (x, _)) = bounds cave
    row         = x - cur
    highEmpty   = head $ dropWhile (rowUsed cave) [row, row - 1..]

emptyRows :: Cave -> Int
emptyRows cave = length $ takeWhile (rowEmpty cave) [0..]

rowEmpty :: Cave -> Int -> Bool
rowEmpty = not .: rowUsed

rowUsed :: Cave -> Int -> Bool
rowUsed cave row = foldl' (||) False $ map (cave !) rowCoords
    where rowCoords = zip (repeat row) [0..6]

fullRow :: Cave -> Int -> Bool
fullRow cave row = foldl' (&&) True $ map (cave !) rowCoords
    where rowCoords = zip (repeat row) [0..6]

validSpot :: [Coord] -> Cave -> Bool
validSpot coords cave = foldr (&&) True $ map isValid coords
    where
    isValid :: Coord -> Bool
    isValid cd = inRange bnds cd && not (cave!cd)
    bnds = bounds cave

blankRows :: Int -> [Bool]
blankRows x = replicate (x * 7) False
origin :: Coord
origin = (0, 0)

horz :: Rock
horz = Rock (zip (repeat 0) [2, 3, 4, 5]) 1 0
plus :: Rock
plus = Rock [(0, 3), (1, 2), (1, 3), (1, 4), (2, 3)] 3 0
ell :: Rock
ell  = Rock [(0, 4), (1, 4), (2, 2), (2, 3), (2, 4)] 3 0
vert :: Rock
vert = Rock (zip [0, 1, 2, 3] $ repeat 2) 4 0
box :: Rock
box  = Rock [(0, 2), (0, 3), (1, 2), (1, 3)] 2 0

rockOrder :: [Rock]
rockOrder = cycle [horz, plus, ell, vert, box]

type Jet = Coord -> Coord
-- Call this with tetris (result of aocParse)
--   which is the parsed input, rockOrder, an initial 0x0 cave
--   matrix, 2022 for the count, 0 as initial height

emptyarr :: Cave
emptyarr = listArray ((1, 1), origin) []
aocParse :: Parser Tetris
aocParse = do
    jets <- some parseJet <* newline <* eof
    pure $ Tetris (cycle jets)
                  rockOrder
                  emptyarr
                  0
                  0
    where
    parseJet :: Parser Jet
    parseJet = choice
        [ char '<' *> pure pushLeft
        , char '>' *> pure pushRight
        ]
    pushLeft  (x, y) = (x, y - 1)
    pushRight (x, y) = (x, y + 1)
