{-# LANGUAGE TupleSections #-}
import Control.Arrow
import Data.List
import Data.Function

solve1 :: [Int] -> Int
solve1 = length . (!!80) . iterate (concatMap next) where
    next 0 = [6, 8]
    next x = [x - 1]

solve2 :: [Int] -> Int
solve2 = sum . map snd . (!!256) . iterate (merge . concatMap next) . map (,1) where
    next (0, c) = [(6, c), (8, c)]
    next (x, c) = [(x - 1, c)]
    merge = map (fst . head &&& sum . map snd) . groupBy ((==) `on` fst) . sort

parse :: String -> [Int]
parse s = read $ "[" ++ s ++ "]"

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
