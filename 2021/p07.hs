import Data.List

solve1 :: [Int] -> Int
solve1 xs = sum $ map (abs . (mid-)) xs where
    n = length xs
    mid = sort xs !! (n `div` 2)

solve2 :: [Int] -> Int
solve2 xs = minimum $ map totalCost [minimum xs .. maximum xs] where
    totalCost x = sum $ map (cost . abs . (x-)) xs
    cost x = x * (x + 1) `div` 2

parse :: String -> [Int]
parse s = read $ "[" ++ s ++ "]"

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
