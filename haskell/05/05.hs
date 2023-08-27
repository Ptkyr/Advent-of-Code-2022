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
    (listArr1 crates, ) <$> some parseMove <* eof
    where
    parseCrates :: Parser [[Crates]]
    parseCrates = do
        (oneCrate `sepBy` char ' ') `endBy` newline
    oneCrate :: Parser Crates
    oneCrate = choice
        [ char '[' *> some upperChar <* char ']'
        , string "   " *> (pure []) -- for the ++ later
        ]
    parseMove :: Parser Move
    parseMove = do
        Move <$> (lexeme "move" *> nat)
             <*> (lexeme "from" *> nat)
             <*> (lexeme "to"   *> nat)
