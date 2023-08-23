main :: IO ()
main = do
    input <- readFile "03/input.txt"
    let sacks = words input
    print $ partOne sacks
    print $ partTwo sacks

priority :: Char -> Int
priority c
    | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A' + 27
    | otherwise            = fromEnum c - fromEnum 'a' + 1

partOne :: [String] -> Int
partOne = sum . map rucks
    where 
    rucks :: String -> Int
    rucks s = common (take h s) (drop h s)
        where
        h = (length s) `div` 2
        common :: String -> String -> Int
        common [] _     = error "Unreachable"
        common (a : as) (bs)
            | elem a bs = priority a
            | otherwise = common as bs

partTwo :: [String] -> Int
partTwo (a : b : c : ss) = sack3 a b c + partTwo ss
    where
    sack3 :: String -> String -> String -> Int
    sack3 [] _ _               = error "Unreachable"
    sack3 (x : xs) y z
        | elem x y && elem x z = priority x
        | otherwise            = sack3 xs y z
partTwo _                      = 0
