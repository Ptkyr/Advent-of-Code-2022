import Utils
--import qualified Data.Map as M
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main = do
    parsed <- parseInput aocParse "16/ex.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print $ solve input "AA"

data Valve = Valve
    { _open :: Bool
    , _flow :: Int
    , _cxns :: [String]
    , _used :: [String]
    } deriving (Show)

type ValveMap = M.Map String Valve

data Tunnels = Tunnels
    { _valves :: ValveMap
    , _timer  :: Int
    , _rate   :: Int
    , _outgas :: Int
    } deriving (Show)

solve :: Tunnels -> String -> Int
solve t@(Tunnels valves timer rate outgas) cur
    | timer == 30 
    = outgas
    | cxns == used
    = total
    | allOpen valves
    = total
    | not open && flow > 0
   -- = trace ("opening " ++ show outgas)
    = maximum $ map (solve opened) cxns
    | otherwise
    = maximum $ map (solve skipped) cxns
    where
    opened  = Tunnels valves' timer' rate' outgas'
    skipped = Tunnels valves timer' rate outgas'
    v@(Valve open flow cxns used) = valves M.! cur
    valves' = M.insert cur v { _open = True } valves
    timer'  = timer + 1
    rate'   = rate + flow
    outgas' = outgas + rate'
    total   = outgas + (30 - timer) * outgas
    bestExit = case unused of 
        [] -> error "Unreachable"
        x  -> maxFlow x
    unused  = cxns \\ used
    maxFlow :: [String] -> String
    maxFlow = maximumBy (compare `on` toFlow)
    toFlow :: String -> Int
    toFlow = _flow . (valves M.!)

allOpen :: ValveMap -> Bool
allOpen = M.foldr (usefulOpen) True
    where
    usefulOpen :: Valve -> Bool -> Bool
    usefulOpen (Valve open flow _ _) b
        = b && (flow == 0 || flow > 0 && open)

aocParse :: Parser Tunnels
aocParse = do
    tunnels <- foldr (yharon M.insert fst snd) M.empty
            <$> some parseValve <* eof
    pure $ Tunnels tunnels 0 0 0
    where
    parseValve :: Parser (String, Valve)
    parseValve = do
        name <- lexeme "Valve" 
             *> lexword
        flow <- lexeme "has flow rate=" 
             *> nat
        cxns <- choice
                [ lexeme "; tunnels lead to valves"
                , lexeme "; tunnel leads to valve"
                ]
             *> lexword `sepBy1` lexeme ","
        pure (name, Valve False flow cxns [])
