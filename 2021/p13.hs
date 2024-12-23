import Data.List

type Point = (Int, Int)
type Fold = Point -> Point

solve1 :: ([Point], [Fold]) -> Int
solve1 (ps, ~(f:_)) = length $ group $ sort $ map f ps

solve2 :: ([Point], [Fold]) -> String
solve2 (ps, fs) = code where
    ps' = foldl' (flip (.)) id fs <$> ps
    mx = maximum $ map fst ps'
    my = maximum $ map snd ps'
    code = unlines [[if (x, y) `elem` ps' then '█' else ' ' | x <- [0..mx]] | y <- [0..my]]

parse :: String -> ([Point], [Fold])
parse s = (map parsePoint ps, map parseFold fs) where
    (ps, _:fs) = break null $ lines s
    parsePoint s = read $ "(" ++ s ++ ")"
    parseFold s = case axis of
        'x' -> foldx
        'y' -> foldy
        _   -> error "!"
      where
        ["fold", "along", axis:'=':num] = words s
        num' = read num
        foldx xy@(x, y) = if x > num' then (2 * num' - x, y) else xy
        foldy xy@(x, y) = if y > num' then (x, 2 * num' - y) else xy

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    putStrLn $ solve2 p
