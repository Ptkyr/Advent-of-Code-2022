import Utils

main :: IO ()
main = do
    parsed <- parseInput aocParse "14/input.txt"
    case parsed of
        Left pError -> putStr $ errorBundlePretty pError
        Right input -> do
            print input

-- get max x, max y
-- make array from 1 1 to max ma
--
type Rock = [Coord]

aocParse :: Parser Rock
aocParse = concat <$> some parseRock <* eof
    where
    parseRock :: Parser Rock
    parseRock = nub
              . concat 
              . mapAdjacent fillLine 
              <$> dbg "rock" (parseCoord `sepBy1` lexeme "->")
    parseCoord :: Parser Coord
    parseCoord = (, ) <$> (lexeme nat <* lexeme ",")
                      <*> lexeme nat
