import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data SnailfishNum = S Int | P SnailfishNum SnailfishNum

snailfishNum :: ReadP SnailfishNum
snailfishNum = single +++ pair where
    single = S . read <$> munch1 isDigit
    pair = between (char '[') (char ']') $ P <$> snailfishNum <* char ',' <*> snailfishNum

explode :: SnailfishNum -> Maybe SnailfishNum
explode = fmap (\(n, _, _) -> n) . go (0 :: Int) where
    go _ (S _)           = Nothing
    go 4 (P (S a) (S b)) = Just (S 0, Just a, Just b)
    go 4 _               = error "not expected"
    go d (P a b) = explodea <|> explodeb where
        explodea = (\(a', l, r) -> (P a' (maybe b (addL b) r), l, Nothing)) <$> go (d + 1) a
        explodeb = (\(b', l, r) -> (P (maybe a (addR a) l) b', Nothing, r)) <$> go (d + 1) b
    addL (S x)   y = S (x + y)
    addL (P a b) y = P (addL a y) b
    addR (S x)   y = S (x + y)
    addR (P a b) y = P a (addR b y)

split :: SnailfishNum -> Maybe SnailfishNum
split (S x)   = P (S x') (S (x - x')) <$ guard (x >= 10) where x' = x `div` 2
split (P a b) = (flip P b <$> split a) <|> (P a <$> split b)

add :: SnailfishNum -> SnailfishNum -> SnailfishNum
add a b = reduce $ P a b where
    reduce n = maybe n reduce $ explode n <|> split n

magnitude :: SnailfishNum -> Int
magnitude (S x)   = x
magnitude (P a b) = 3 * magnitude a + 2 * magnitude b

solve1 :: [SnailfishNum] -> Int
solve1 = magnitude . foldl1 add

solve2 :: [SnailfishNum] -> Int
solve2 = maximum . map (magnitude . uncurry add) . ordPairs

ordPairs :: [a] -> [(a, a)]
ordPairs xs = [(x, x') | (ls, x:rs) <- zip (inits xs) (tails xs), x' <- ls ++ rs]

parse :: String -> [SnailfishNum]
parse = map (fst . head . readP_to_S snailfishNum) . lines

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
