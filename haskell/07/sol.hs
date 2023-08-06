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
        Nothing            -> mkFS f xs
        Just num           -> mkFS f {ch = new : ch f} xs
            where new = File num "" [] Nothing
    _ | nm f /= "/"        -> unroll p [] -- Done parsing, make / root
      | otherwise          -> szSet f     -- Return root 
    where 
        p = pn f
        -- unroll -> after making a dir, insert into parent
        unroll :: Maybe File -> [String] -> File
        unroll (Just d) cs = mkFS d {ch = (szSet f) : ch d} cs
        unroll Nothing  _  = rootFile -- Error, unreachable
        -- szSet -> after making a dir, set its size
        szSet :: File -> File
        szSet d = d {sz = szCh d, pn = Nothing}
            where
                szCh :: File -> Int
                szCh = sum . map sz . ch

partOne :: File -> Int
partOne f = isSmall f + (sum . map partOne . ch $ f)
    where 
        isSmall :: File -> Int
        isSmall (File _ _ [] _) = 0 -- Only count directories
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
