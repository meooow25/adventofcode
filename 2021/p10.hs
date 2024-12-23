{-# LANGUAGE LambdaCase #-}
import Data.List
import Data.Maybe

close :: Char -> Char
close = \case
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    '<' -> '>'
    _   -> error "!"

solve1 :: [String] -> Int
solve1 = sum . map (go []) where
    go _ [] = 0
    go ys (x:xs)
        | x `elem` "([{<"             = go (x:ys) xs
        | (y:ys') <- ys, x == close y = go ys' xs
        | otherwise                   = score x
    score = \case
        ')' -> 3
        ']' -> 57
        '}' -> 1197
        '>' -> 25137
        _   -> error "!"

solve2 :: [String] -> Int
solve2 ls = sort scores !! (length scores `div` 2) where
    scores = mapMaybe (go []) ls
    go ys [] = Just $ foldl' (\acc x -> acc * 5 + score (close x)) 0 ys
    go ys (x:xs)
        | x `elem` "([{<"             = go (x:ys) xs
        | (y:ys') <- ys, x == close y = go ys' xs
        | otherwise                   = Nothing
    score = \case
        ')' -> 1
        ']' -> 2
        '}' -> 3
        '>' -> 4
        _   -> error "!"

main :: IO ()
main = do
    p <- lines <$> getContents
    print $ solve1 p
    print $ solve2 p
