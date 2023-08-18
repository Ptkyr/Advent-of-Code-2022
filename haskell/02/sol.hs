main :: IO ()
main = do
    input <- readFile "02/input.txt"
    let rps = words $ input
    print . partOne $ rps
    print . partTwo $ rps

partOne :: [String] -> Int
partOne ("A" : "X" : s) = 4 + partOne s
partOne ("A" : "Y" : s) = 8 + partOne s
partOne ("A" : "Z" : s) = 3 + partOne s
partOne ("B" : "X" : s) = 1 + partOne s
partOne ("B" : "Y" : s) = 5 + partOne s
partOne ("B" : "Z" : s) = 9 + partOne s
partOne ("C" : "X" : s) = 7 + partOne s
partOne ("C" : "Y" : s) = 2 + partOne s
partOne ("C" : "Z" : s) = 6 + partOne s
partOne _               = 0

partTwo :: [String] -> Int
partTwo ("A" : "X" : s) = 3 + partTwo s
partTwo ("A" : "Y" : s) = 4 + partTwo s
partTwo ("A" : "Z" : s) = 8 + partTwo s
partTwo ("B" : "X" : s) = 1 + partTwo s
partTwo ("B" : "Y" : s) = 5 + partTwo s
partTwo ("B" : "Z" : s) = 9 + partTwo s
partTwo ("C" : "X" : s) = 2 + partTwo s
partTwo ("C" : "Y" : s) = 6 + partTwo s
partTwo ("C" : "Z" : s) = 7 + partTwo s
partTwo _               = 0
