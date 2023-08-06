import Data.List (sort, foldl')
import Text.Read (readMaybe)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let ignoreRoot = drop 3 . words $ input
    let system = mkFS rootFile $ ignoreRoot
    print . partOne $ system
    print . partTwo $ system

data File = File
    { sz :: Int
    , nm :: String
    , ch :: [File]
    , pn :: Maybe File
    }

rootFile :: File
rootFile = File 0 "/" [] Nothing

mkFS :: File -> [String] -> File
mkFS f cmd = case cmd of
    "$" : "cd" : ".." : xs -> unroll p xs
    "$" : "cd" : _ : xs    -> mkFS (File 0 "" [] $ Just f) xs
    x : _ : xs -> case readMaybe x of
        Just num           -> mkFS f {sz = num + sz f} xs
        Nothing            -> mkFS f xs
    _ | nm f /= "/"        -> unroll p [] -- Done parsing, make / root
      | otherwise          -> f           -- Return root 
    where 
        p = pn f
        -- unroll -> after making a dir, insert into parent
        unroll :: Maybe File -> [String] -> File
        unroll (Just d) cs = mkFS d {ch = f : ch d, sz = sz d + sz f} cs
        unroll Nothing  _  = rootFile -- Error, unreachable

partOne :: File -> Int
partOne f = isSmall f + (sum . map partOne . ch $ f)
    where 
        isSmall :: File -> Int
        isSmall (File x _ _ _)
            | x <= 100000 = x
            | otherwise   = 0
        
p2min :: File -> Int
p2min rt = 30000000 - 70000000 + sz rt

partTwo :: File -> Int
partTwo rt = p2FixT (ftmin rt) rt
    where
        p2FixT :: (Int -> Int -> Int) -> File -> Int
        -- Order moot, but foldl' is faster
        p2FixT m f = foldl' m (sz f) $ map (p2FixT m) . ch $ f
        -- ftmin -> wrapper
        ftmin :: File -> Int -> Int -> Int
        ftmin = gtmin . p2min
        -- gtmin -> take min of numbers above t, 0 iff both below
        gtmin :: Int -> Int -> Int -> Int
        gtmin t x y = case sort . filter (> t) $ [x, y] of
            m : _ -> m
            []    -> 0
