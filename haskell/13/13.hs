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

instance (Ord a) => Ord (Packet a) where
    compare (Igr x) (Igr y)          = compare x y
    compare (Igr x) s@(Some _)       = compare (Some [Igr x]) s
    compare s@(Some _) (Igr x)       = compare s (Some [Igr x])
    compare (Some []) (Some [])      = EQ
    compare (Some []) (Some (_ : _)) = LT
    compare (Some (_ : _)) (Some []) = GT
    compare (Some (x : xs)) (Some (y : ys)) 
        | res == EQ                  = compare (Some xs) (Some ys)
        | otherwise                  = res
        where res = compare x y

type Pint = Packet Int
type Pair = (Pint, Pint)

partOne :: Arr Pint -> Int
partOne = sum 
        . map p1Order 
        . assocs 
        . listArr1 
        . toPairs 
        . elems
    where
    p1Order :: (Int, Pair) -> Int
    p1Order (x, (p1, p2)) 
        | p1 < p2   = x
        | otherwise = 0

partTwo :: Arr Pint -> Int
partTwo ap = (indexByValue div2 arr) 
           * (indexByValue div6 arr)
    where
    div2 = Some [Some [Igr 2]]
    div6 = Some [Some [Igr 6]]
    arr = listArr1
        . sort 
        . ((++) [div2, div6])
        $ elems ap

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
