{-# LANGUAGE BangPatterns, TupleSections #-}
import Control.Monad
import Data.List
import qualified Data.Map as M

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (!a1, !a2) (!b1, !b2) = (a1 + b1, a2 + b2)

mul :: Int -> (Int, Int) -> (Int, Int)
mul x (!a1, !a2) = (a1 * x, a2 * x)

solve1 :: (Int, Int) -> Int
solve1 (a, b) = go (a, 0) (b, 0) 1 0 where
    go (pos1, score1) (pos2, score2) dice diceCnt
        | score2 >= 1000 = score1 * diceCnt
        | otherwise      = go (pos2, score2) (pos1', score1') dice' (diceCnt + 3)
      where
        roll    = 3 * dice + 3
        pos1'   = (pos1 + roll - 1) `mod` 10 + 1
        score1' = score1 + pos1'
        dice'   = (dice + 3 - 1) `mod` 100 + 1

solve2 :: (Int, Int) -> Int
solve2 (a, b) = uncurry max $ go (a, 0, (1, 0)) (b, 0, (0, 1)) where
    go (pos1, score1, pt1) (pos2, score2, pt2)
        | score2 >= 21 = pt2
        | otherwise    = foldl' add (0, 0) $ map f counts
      where
        f (roll, rollCnt) = mul rollCnt $ go (pos2, score2, pt2) (pos1', score1', pt1) where
            pos1'   = (pos1 + roll - 1) `mod` 10 + 1
            score1' = score1 + pos1'
    counts = M.assocs $ M.fromListWith (+) $ map ((,1) . sum) $ replicateM 3 [1, 2, 3]

parse :: String -> (Int, Int)
parse s = (read $ last $ words l1, read $ last $ words l2) where [l1, l2] = lines s

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
