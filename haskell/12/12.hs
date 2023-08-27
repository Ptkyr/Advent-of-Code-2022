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
partOne d = dijkstra (== _end d) p1Adj d
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
    let idxS  = indexByValue 'S' graph
    let idxE  = indexByValue 'E' graph
    pure $ Dijkstra idxS idxE (graph // [(idxS, 'a'), (idxE, 'z')])
         $ listArray (bounds graph) (repeat maxBound)
