import Data.List (sort, foldl')
import Text.Read (readMaybe)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let system = mkFS rootDir . drop 3 . words $ input
    print . partOne $ system
    print . partTwo $ system

-- Files won't actually exist, I only need their sizes
-- Names also don't really matter, but having / is nice
data Dir = Dir
    { sz :: Int
    , nm :: String
    , ch :: [Dir]
    , pn :: Maybe Dir
    }

rootDir :: Dir
rootDir = Dir 0 "/" [] Nothing

mkFS :: Dir -> [String] -> Dir
mkFS f cmd = case cmd of
    "$" : "cd" : ".." : xs -> unroll p xs
    "$" : "cd" : _ : xs    -> mkFS (Dir 0 "" [] $ Just f) xs
    x : _ : xs -> case readMaybe x of
        Just num           -> mkFS f {sz = num + sz f} xs
        Nothing            -> mkFS f xs
    _ | nm f /= "/"        -> unroll p [] -- Done parsing, make / root
      | otherwise          -> f           -- Return root 
    where 
        p = pn f
        -- unroll; after making a dir, insert into parent
        unroll :: Maybe Dir -> [String] -> Dir
        unroll Nothing  _  = error "Unreachable"
        unroll (Just d) cs = mkFS newdir cs
            where newf = f {pn = Nothing}
                  newdir = d {ch = newf : ch d, sz = sz newf + sz d}

partOne :: Dir -> Int
partOne f = isSmall f + (sum . map partOne . ch $ f)
    where 
        isSmall :: Dir -> Int
        isSmall t = if x <= 100000 then x else 0
            where x = sz t
        
p2min :: Dir -> Int
p2min rt = 30000000 - 70000000 + sz rt

partTwo :: Dir -> Int
partTwo rt = p2FixT (gtmin . p2min $ rt) rt
    where
        p2FixT :: (Int -> Int -> Int) -> Dir -> Int
        -- Order moot, but foldl' is faster
        p2FixT m f = foldl' m (sz f) $ map (p2FixT m) . ch $ f
        -- gtmin; take min of x y above t, 0 iff both below
        gtmin :: Int -> Int -> Int -> Int
        gtmin t x y = case sort . filter (> t) $ [x, y] of
            m : _ -> m
            []    -> 0
