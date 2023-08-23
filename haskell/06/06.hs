import Utils

main :: IO ()
main = do
    input <- readFile "06/input.txt"
    print $ partOne input
    print $ partTwo input

partOne :: String -> Int
partOne = uniq 4 4

partTwo :: String -> Int
partTwo = uniq 14 14

uniq :: Int -> Int -> String -> Int
uniq _ _ [] = 0
uniq c x str@(_ : ss)
    | u == c    = x 
    | otherwise = uniq c (x + 1) ss
    where 
    u = length . group . sort $ take c str
