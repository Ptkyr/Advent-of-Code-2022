import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "07/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ partOne input
            print $ partTwo input

-- Files won't actually exist, I only need their sizes
-- Names also don't really matter, but having / is nice
data Dir = Dir
    { _size :: Int
    , _name :: String
    , _subs :: [Dir]
    , _prnt :: Maybe Dir
    }

rootDir :: Dir
rootDir = Dir 0 "/" [] Nothing

aocParse :: Parser Dir
aocParse = lexeme "$ cd /" *> parseDir rootDir
    where
    parseDir :: Dir -> Parser Dir
    parseDir dir = choice
        [ lexeme "$ ls"    *> parseDir dir
        , lexeme "$ cd .." *> parseDir (unroll p)
        , lexeme "$ cd"    *> lexeme (some lowerChar) 
                           *> parseDir (Dir 0 "" [] $ Just dir)
        , lexeme "dir"     *> lexeme (some lowerChar) 
                           *> parseDir dir
        , do 
            i <- lexeme nat <* lexeme (some (lowerChar <|> char '.'))
            parseDir dir {_size = i + _size dir}
        , eof *> case _name dir of
            "/" -> pure $ dir
            _   -> parseDir (unroll p)
        ]
        where 
        p    = _prnt dir
        newf = dir {_prnt = Nothing}
        -- unroll; after making a dir, insert into parent
        unroll :: Maybe Dir -> Dir
        unroll Nothing  = error "Unreachable"
        unroll (Just d) = newdir
            where newdir = d {_subs = newf : _subs d
                             ,_size = _size newf + _size d}

partOne :: Dir -> Int
partOne f = isSmall f + (sum . map partOne $ _subs f)
    where 
    isSmall :: Dir -> Int
    isSmall t = if x <= 100000 then x else 0
        where x = _size t
        
p2min :: Dir -> Int
p2min rt = 30000000 - 70000000 + _size rt

partTwo :: Dir -> Int
partTwo rt = p2FixT (gtmin $ p2min rt) rt
    where
    p2FixT :: (Int -> Int -> Int) -> Dir -> Int
    -- Order moot, but foldl' is faster
    p2FixT m f = foldl' m (_size f) $ map (p2FixT m) $ _subs f
    -- gtmin; take min of x y above t, 0 iff both below
    gtmin :: Int -> Int -> Int -> Int
    gtmin t x y = case sort $ filter (> t) [x, y] of
        m : _ -> m
        []    -> 0
