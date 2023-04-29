main :: IO ()
main = do
    input <- readFile "input.txt"
    let elves = parse $ input
    print . partOne $ elves
    print . partTwo $ elves

-- Basically splitOn
parse :: String -> [Int]
parse s = case break (\x -> x == '\n' || 
                            x == ','  || 
                            x == '-') s of
    (a, _ : b) -> read a : parse b
    (a, _)     -> [read a]

partOne :: [Int] -> Int
partOne (x : y : z : w : ss) = contains x y z w + partOne ss
    where
    contains :: Int -> Int -> Int -> Int -> Int
    contains a1 a2 b1 b2
        | a1 <= b1 && b2 <= a2 = 1
        | b1 <= a1 && a2 <= b2 = 1
        | otherwise            = 0
partOne _                    = 0

partTwo :: [Int] -> Int
partTwo (x : y : z : w : ss) = overlaps x y z w + partTwo ss
    where
    overlaps :: Int -> Int -> Int -> Int -> Int
    overlaps a1 a2 b1 b2
        | a2 <= b2 && a2 >= b1 = 1
        | a2 >= b2 && a1 <= b2 = 1
        | otherwise            = 0
partTwo _                    = 0
