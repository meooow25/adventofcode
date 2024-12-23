import Control.Monad
import Data.Foldable
import Data.List

type Board = [[Int]]

solve1 :: ([Int], [Board]) -> Maybe Int
solve1 (xs, boards) = asum $ map getFirst $ inits xs where
    getFirst xs' = asum $ map getScore boards where
        test = any (all (`elem` xs'))
        getScore board = score <$ guard (test board || test (transpose board)) where
            score = last xs' * sum (filter (`notElem` xs') $ concat board)

solve2 :: ([Int], [Board]) -> Maybe Int
solve2 (xs, boards) = asum $ map getFirst $ reverse $ inits xs where
    getFirst xs' = asum $ map getScore boards where
        testCur = any (all (`elem` xs'))
        testLast = not . any (all (`elem` init xs'))
        getScore board = score <$ guard ok where
            ok = (testCur board || testCur (transpose board)) && testLast board && testLast (transpose board)
            score = last xs' * sum (filter (`notElem` xs') $ concat board)

parse :: String -> ([Int], [Board])
parse s = (xs, boards) where
    ls = lines s
    xs = read $ "[" ++ head ls ++ "]"
    boards = map tail $ chunksOf 6 $ map (map read . words) $ tail ls

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go where
    go [] = []
    go xs = xs' : go xs'' where (xs', xs'') = splitAt n xs

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
