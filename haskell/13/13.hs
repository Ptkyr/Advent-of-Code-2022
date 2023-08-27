import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "13/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

data Packet a = Igr a | Some [Packet a]
    deriving (Show, Eq)

type Pint = Packet Int
type Pair = (Pint, Pint)

partOne :: Arr Pint -> Int
partOne = sum . map p1Order . assocs . listArr1 . toPairs . elems
    where
    p1Order :: (Int, Pair) -> Int
    p1Order (x, (p1, p2)) = case cmp p1 p2 of
        LT -> x
        GT -> 0
        EQ -> error "Unreachable"

partTwo :: Arr Pint -> Int
partTwo ap = (indexByValue div2 arr) * (indexByValue div6 arr)
    where
    div2 = Some [Some [Igr 2]]
    div6 = Some [Some [Igr 6]]
    arr = listArr1
        . sortBy cmp 
        . ((++) [div2, div6])
        $ elems ap

cmp :: (Ord a) => Packet a -> Packet a -> Ordering
cmp (Igr x) (Igr y)          = compare x y
cmp (Igr x) s@(Some _)       = cmp (Some [Igr x]) s
cmp s@(Some _) (Igr x)       = cmp s (Some [Igr x])
cmp (Some []) (Some [])      = EQ
cmp (Some []) (Some (_ : _)) = LT
cmp (Some (_ : _)) (Some []) = GT
cmp (Some (x : xs)) (Some (y : ys)) 
    | res == EQ              = cmp (Some xs) (Some ys)
    | otherwise              = res
    where res = cmp x y

aocParse :: Parser (Arr Pint)
aocParse = do
    listArr1 <$> some parsePacket <* eof
    where
    parsePacket :: Parser Pint
    parsePacket = do
        Some <$> brackets (oneElem `sepBy` char ',')
        where
        oneElem :: Parser Pint
        oneElem = Igr <$> lexeme nat <|> parsePacket
