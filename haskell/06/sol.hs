import Data.List

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . partOne $ input
    print . partTwo $ input

partOne :: String -> Int
partOne = uniq 4 4

partTwo :: String -> Int
partTwo = uniq 14 14

uniq :: Int -> Int -> String -> Int
uniq _ _ [] = 0
uniq c x (s : ss)
    | u == c    = x 
    | otherwise = uniq c (x + 1) ss
        where 
        u = length . group . sort . take c $ s : ss
