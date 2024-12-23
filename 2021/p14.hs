{-# LANGUAGE TupleSections #-}
import Data.List
import qualified Data.Map as M

type Replace = (Char, Char) -> Maybe Char

solve1 :: (String, Replace) -> Int
solve1 (template, replace) = maximum freqs - minimum freqs where
    step (a:b:xs) = case replace (a, b) of
        Nothing -> a : step (b:xs)
        Just c  -> a : c : step (b:xs)
    step xs = xs
    t = iterate step template !! 10
    freqs = map length $ group $ sort t

solve2 :: (String, Replace) -> Int
solve2 (template, replace) = maximum freqs - minimum freqs where
    nxt cur@((a, b), cnt) = case replace (a, b) of
        Nothing -> [cur]
        Just c  -> [((a, c), cnt), ((c, b), cnt)]
    t = "*" ++ template ++ "*"
    freqs =
        map ((`div` 2) . snd) $
        filter ((/='*') . fst) $
        combine $ concatMap split $
        iterate (combine . concatMap nxt) (map (,1) $ zip t (tail t)) !! 40

    combine :: Ord a => [(a, Int)] -> [(a, Int)]
    combine = M.assocs . M.fromListWith (+)
    split ((a, b), cnt) = [(a, cnt), (b, cnt)]

parse :: String -> (String, Replace)
parse s = (p, (`lookup` rules)) where
    p:"":s' = lines s
    rules = map parseRule s'
    parseRule s = ((a, b), c) where
        [[a, b], "->", [c]] = words s

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
