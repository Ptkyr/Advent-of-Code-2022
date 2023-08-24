import Utils

main :: IO ()
main = do
    input <- readFile "05/input.txt"
    let config = takeWhile (/= '1') input
    let cmds = parseCmds $ dropWhile (\x -> x /= 'm') input
    let initBoxes = parseConfig (replicate 9 []) . init $ parse config
    print . partOne initBoxes $ cmds
    print . partTwo initBoxes $ cmds
    parsed <- parseInput aocParse "05/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> print $ fst input

type Info   = ([Crates], [Move])
type Crates = String
data Move = Move
    { _num :: Int
    , _frm :: Int
    , _to  :: Int
    } deriving (Show)

aocParse :: Parser Info
aocParse = do
    (, ) <$> foldl' (zipWith (++)) (repeat []) <$> parseCrates
         <*> (space1 *> some (lexeme nat) *> some parseMove <* eof)
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
            , string "   " *> (pure []) -- annoying
            ]

-- <Parsing>
-- Initialize boxes
parseConfig :: [String] -> [String] -> [String]
parseConfig boxes []      = boxes
parseConfig boxes (s : t) = parseConfig (boxLine boxes s) t
    where
    boxLine :: [String] -> String -> [String]
    boxLine (b : bs) (_ : ' ' : _ : ss) = b : boxLine bs (tail ss)
    boxLine (b : bs) (_ : a : _ : ss)   = (b ++ [a]) : boxLine bs (tail ss)
    boxLine _ _                         = []

parse :: String -> [String]
parse s = case break (== '\n') s of
    (a, _ : b) -> a : parse b
    (a, _)     -> [a]

parseCmds :: String -> [Int]
parseCmds s = map read $ filter (\x -> isDigit $ head x) (words s)
-- </Parsing>

partOne :: [String] -> [Int] -> String
partOne = solve popFor

partTwo :: [String] -> [Int] -> String
partTwo = solve pushFor

solve :: (Int -> (String, String) -> (String, String)) -> [String] -> [Int] -> String
solve fn s cs = map head (tf s cs)
    where 
    tf :: [String] -> [Int] -> [String]
    tf boxes (c : f : t : cmds) = tf (move boxes fn c f t) cmds
    tf boxes _                  = boxes

move :: [String] -> (Int -> (String, String) -> (String, String)) -> Int -> Int -> Int -> [String]
move boxes pf cnt fm to = moveMerge boxes (pf cnt (a, b)) fm to
    where
    a = boxes !! (fm - 1)
    b = boxes !! (to - 1)
    moveMerge :: [String] -> (String, String) -> Int -> Int -> [String]
    moveMerge (_ : xs) (f, t) 1 ti = f : moveMerge xs (f, t) 0 (ti - 1)
    moveMerge (_ : xs) (f, t) fi 1 = t : moveMerge xs (f, t) (fi - 1) 0
    moveMerge (x : xs) tpl fi ti   = x : moveMerge xs tpl (fi - 1) (ti - 1)
    moveMerge xs _ 0 0             = xs
    moveMerge [] _ _ _             = []

popFor :: Int -> (String, String) -> (String, String)
popFor 0 x = x
popFor n x = popFor (n - 1) (popper x)
    where
    popper :: (String, String) -> (String, String)
    popper (a : as, bs) = (as, a : bs)
    popper tpl          = tpl

pushFor :: Int -> (String, String) -> (String, String)
pushFor n (a, b) = (drop n a, take n a ++ b)
