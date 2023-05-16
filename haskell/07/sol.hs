main :: IO ()
main = do
    input <- readFile "input.txt"
    print . words $ input

data File = File
    { name :: String
    , size :: Int
    , subs :: [File]
    , prnt :: Maybe File
    } deriving (Show)

buildFile :: File -> [String] -> File
buildFile f ("$" : "cd" : ".." : s) = case prnt f of
    Just p  -> buildFile (p { subs = replaceFile (subs p) f }) s
    Nothing -> f
buildFile f ("$" : "cd" : d : s)    = buildFile (findFile (subs f) d) s
buildFile f ("$" : "ls" : s)        = buildFile f s
buildFile f ("dir" : d : s)         = buildFile (addFile f (File d 0 [] (Just f))) s
buildFile f (x : d : s)             = buildFile (addFile f (File d (read x) [] (Just f))) s
buildFile f _                       = f

addFile :: File -> File -> File
addFile p f = p { subs = f : (subs p) }

replaceFile :: [File] -> File -> [File]
replaceFile [] _ = []
replaceFile (s : ss) f
    | name s == name f = f : ss
    | otherwise        = s : replaceFile ss f

findFile :: [File] -> String -> File
findFile (f : []) _ = f
findFile (f : fs) s
    | name f == s = f
    | otherwise   = findFile fs s

setFileSize :: File -> File
setFileSize f = case subs f of
    []        -> f
    otherwise -> f { size = sum . map size . map setFileSize $ (subs f) }
