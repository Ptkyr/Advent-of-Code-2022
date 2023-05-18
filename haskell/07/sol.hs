main :: IO ()
main = do
    input <- readFile "input.txt"
    let tokens = drop 3 . words $ input
    let filesystem = (buildFile (File "/" 0 0 []) tokens)
    --print . partOne $ filesystem
    print filesystem

data File = File
    { name :: String
    , size :: Int
    , scnt :: Int
    , subs :: [File]
    } deriving (Show)

-- Need to return modified input stream
buildFile :: File -> [String] -> (File, [String])
buildFile f ("$" : "cd" : ".." : s) = f
buildFile f ("$" : "cd" : d : s)    = f { scnt = scnt f + 1, subs = (buildFile (File d 0 0 []) s) : (subs f)}
buildFile f ("$" : "ls" : s)        = buildFile f s
buildFile f ("dir" : d : s)         = buildFile f s
buildFile f (x : d : s)             = buildFile (f { scnt = scnt f + 1, subs = (File d (read x) 0 []) : (subs f)}) s
buildFile f _                       = f

setFileSize :: File -> File
setFileSize f = case subs f of
    []        -> f
    otherwise -> f { size = sum . map size . map setFileSize $ (subs f) }

partOne :: File -> Int
partOne f = isSmall f + (sum . map partOne $ (subs f))
    where
    isSmall :: File -> Int
    isSmall f
        | size f <= 2000000 = 1
        | otherwise        = 0
