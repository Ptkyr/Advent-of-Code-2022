import Utils
import Data.Array

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
partOne = extract reverse

partTwo :: Info -> String
partTwo = extract id

extract :: (Crates -> Crates) -> Info -> Crates
extract func = map head . elems . execute
    where
    execute :: Info -> Arr Crates
    execute (ac, [])     = ac
    execute (ac, m : ms) = execute (doMove m ac, ms)
    doMove :: Move -> Arr Crates -> Arr Crates
    doMove (Move num from to) ac 
        = ac // [(from, newFrom), (to, newTo)]
        where
        fromCrates        = ac!from
        toCrates          = ac!to
        (moving, newFrom) = splitAt num fromCrates
        newTo             = func moving ++ toCrates
    
aocParse :: Parser Info
aocParse = do
    crates <- foldl' (zipWith (++)) (repeat []) <$> parseCrates
    void space1 <* some nat -- eat the 1 2 ... 9
    (listArr1 crates, ) <$> some parseMove <* eof
    where
    parseCrates :: Parser [[Crates]]
    parseCrates = (oneCrate `sepBy1` char ' ') `endBy` newline
    oneCrate :: Parser Crates
    oneCrate = choice
        [ char '[' *> word <* char ']'
        , string "   " *> (pure []) -- for the ++ zip later
        ]
    parseMove :: Parser Move
    parseMove = Move <$> (lexeme "move" *> nat)
                     <*> (lexeme "from" *> nat)
                     <*> (lexeme "to"   *> nat)
