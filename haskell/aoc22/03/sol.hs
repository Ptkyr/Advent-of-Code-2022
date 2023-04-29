main :: IO ()
main = do
    input <- readFile "input.txt"
    let sacks = words input
    print . partOne $ sacks
    print . partTwo $ sacks

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
        common [] _     = -56
        common (a : as) (bs)
            | elem a bs = priority a
            | otherwise = common as bs

partTwo :: [String] -> Int
partTwo []               = 0
partTwo (_ : [])         = 0 -- Just for complete
partTwo (_ : _ : [])     = 0 --  pattern matching
partTwo (a : b : c : ss) = sack3 a b c + partTwo ss
    where
    sack3 :: String -> String -> String -> Int
    sack3 [] _ _               = -56
    sack3 (x : xs) y z
        | elem x y && elem x z = priority x
        | otherwise            = sack3 xs y z
