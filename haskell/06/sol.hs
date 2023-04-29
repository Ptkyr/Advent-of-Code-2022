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
uniq c x str
    | u == c    = x 
    | otherwise = uniq c (x + 1) (tail str)
        where 
        u = length . group . sort . take c $ str
