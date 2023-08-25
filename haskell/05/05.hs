import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "05/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

type Info   = (Arr Crates, [Move])
type Crates = String
data Move = Move
    { _num :: Int
    , _frm :: Int
    , _to  :: Int
    }

partOne :: Info -> String
partOne = map head . elems . execute reverse

partTwo :: Info -> String
partTwo = map head . elems . execute id

execute :: (Crates -> Crates) -> Info -> Arr Crates
execute _ (ac, [])        = ac
execute func (ac, m : ms) = execute func (doMove func m ac, ms)

doMove :: (Crates -> Crates) -> Move -> Arr Crates -> Arr Crates
doMove func (Move num from to) ac = ac // [(from, newFrom), (to, newTo)]
    where
    fromCrates        = ac!from
    toCrates          = ac!to
    (moving, newFrom) = splitAt num fromCrates
    newTo             = func moving ++ toCrates
    
aocParse :: Parser Info
aocParse = do
    crates <- foldl' (zipWith (++)) (repeat []) <$> parseCrates
    void space1 <* some (lexeme nat) -- eat the 1 2 ... 9
    moves  <- some parseMove <* eof
    pure (listArray (1, length crates) crates, moves)
    where
    parseMove :: Parser Move
    parseMove = do
        Move <$> (lexeme "move" *> nat)
             <*> (lexeme "from" *> nat)
             <*> (lexeme "to"   *> nat)
    parseCrates :: Parser [[Crates]]
    parseCrates = do
        (parseBox `sepBy` char ' ') `endBy` newline
        where
        parseBox :: Parser Crates
        parseBox = choice
            [ char '[' *> some upperChar <* char ']'
            , string "   " *> (pure []) -- annoying but I gotta
            ]
