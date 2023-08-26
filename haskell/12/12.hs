import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "12/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

partOne :: Dijkstra -> Int
partOne d = dijkstra (\x -> x == _end d) p1Adj d
    where
    p1Adj :: Node -> Node -> Bool
    p1Adj center nbr = fromEnum center - fromEnum nbr >= -1

partTwo :: Dijkstra -> Int
partTwo d = dijkstra (\x -> (_graph d)!x == 'a') p2Adj d {_start = _end d}
    where
    p2Adj :: Node -> Node -> Bool
    p2Adj center nbr = fromEnum center - fromEnum nbr <= 1

aocParse :: Parser Dijkstra
aocParse = do
    grid <- some letterChar `endBy` newline <* eof
    let graph = listArr2D1 id grid
    let lst = assocs graph
    let startIdx = start lst
    let endIdx = end lst
    let costs = listArray (bounds graph) (repeat maxBound)
    pure $ Dijkstra startIdx endIdx (graph // [(startIdx, 'a'), (endIdx, 'z')])
         $ costs // [(startIdx, 0)]
    where
    start :: [(Coord, Node)] -> Coord
    start = fst . head . filter (\n -> snd n == 'S')
    end :: [(Coord, Node)] -> Coord
    end = fst . head . filter (\n -> snd n == 'E')
