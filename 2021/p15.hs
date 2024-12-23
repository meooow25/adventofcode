import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)
type Grid = M.Map Point Int

neighbors :: Point -> [Point]
neighbors (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

solve1 :: (Int, Int, Grid) -> Int
solve1 (n, m, g) = final M.! (n, m) where
    dijkstra (dist, pq) = case S.minView pq of
        Nothing -> dist
        Just ((d, xy), pq') -> dijkstra $ foldl' upd (dist, pq') $ neighbors xy where
            upd dp@(dist', pq'') xy'
                | maybe True (<=new) maybeCur = dp
                | otherwise = (M.insert xy' new dist', S.insert (new, xy') $ S.delete (cur, xy') pq'')
              where
                maybeCur = dist' M.!? xy'
                cur = fromJust maybeCur
                new = d + g M.! xy'
    final = dijkstra (M.insert (1, 1) 0 $ M.map (const $ maxBound `div` 2) g, S.singleton (0, (1, 1)))

solve2 :: (Int, Int, Grid) -> Int
solve2 (n, m, g) = solve1 (5 * n, 5 * m, scaleUp g) where
    scaleUp = M.unions . concatMap (take 5 . iterate upy) . take 5 . iterate upx where
        upc = (+1) . (`mod` 9)
        upx = M.mapKeys (first  (+n)) . fmap upc
        upy = M.mapKeys (second (+m)) . fmap upc

parse :: String -> (Int, Int, Grid)
parse s = (n, m, M.fromList $ zip ((,) <$> [1..n] <*> [1..m]) (concat xss)) where
    xss = map (map digitToInt) $ lines s
    n = length xss
    m = length $ head xss

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
