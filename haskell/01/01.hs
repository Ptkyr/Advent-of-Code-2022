import Data.List

main :: IO ()
main = do
    input <- readFile "01/input.txt"
    let elves = cals 0 . parse $ input
    print . partOne $ elves
    print . partTwo $ elves

-- Basically splitOn newline
parse :: String -> [String]
parse s = case break (== '\n') s of
    (a, _ : b) -> a : parse b
    (a, _)     -> [a]

-- Convert to each elf's calorie count
cals :: Int -> [String] -> [Int]
cals _ []        = []
cals a ("" : xs) = a : cals 0 xs
cals a (x : xs)  = cals (a + read x) xs

partOne :: [Int] -> Int
partOne = maximum

partTwo :: [Int] -> Int
partTwo = sum . take 3 . sortBy (flip compare)
