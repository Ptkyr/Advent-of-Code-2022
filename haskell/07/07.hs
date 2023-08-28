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

partOne :: Dir -> Int
partOne f = isSmall f + (sum . map partOne $ _subs f)
    where 
    isSmall :: Dir -> Int
    isSmall t = if x <= 100000 then x else 0
        where x = _size t
        
partTwo :: Dir -> Int
partTwo rt = p2FixT (gtmin sizeTarget) rt
    where
    sizeTarget = 30000000 - 70000000 + _size rt
    p2FixT :: (Int -> Int -> Int) -> Dir -> Int
    -- Order moot, but foldl' is faster
    p2FixT m f = foldl' m (_size f) $ map (p2FixT m) $ _subs f
    -- gtmin; take min of x y above t, 0 iff both below
    gtmin :: Int -> Int -> Int -> Int
    gtmin t x y = case sort $ filter (> t) [x, y] of
        m : _ -> m
        []    -> 0

aocParse :: Parser Dir
aocParse = lexeme "$ cd /" *> parseDir (Dir 0 "/" [] Nothing)
    where
    parseDir :: Dir -> Parser Dir
    parseDir dir = choice
        [ lexeme "$ ls"    *> skip
        , lexeme "$ cd .." *> back
        , lexeme "$ cd" 
          *> lexword       *> parseDir (Dir 0 "" [] $ Just dir)
        , lexeme "dir" 
          *> lexword       *> skip
        , do 
            i <- nat 
            void fName     *> parseDir dir {_size = _size dir + i}
        , eof *> case _name dir of
            "/"            -> pure dir
            _              -> back
        ]
        where 
        skip = parseDir dir
        back = parseDir $ unroll p
        p    = _prnt dir
        newf = dir {_prnt = Nothing}
        -- unroll; after making a dir, insert into parent
        unroll :: Maybe Dir -> Dir
        unroll Nothing  = error "Unreachable"
        unroll (Just d) = newdir
            where newdir = d {_subs = newf : _subs d
                             ,_size = _size newf + _size d}
        fName :: Parser String
        fName = lexeme (some $ letterChar <|> char '.')
