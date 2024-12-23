import Data.Char
import Data.List
import Data.Graph
import Data.Maybe
import qualified Data.Map as M

type Point = (Int, Int)

neighbors :: Point -> [Point]
neighbors (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

solve1 :: M.Map Point Int -> Int
solve1 g = sum $ map ((+1) . snd) $ filter lowPoint $ M.assocs g where
    lowPoint (xy, d) = all (>d) $ mapMaybe (g M.!?) $ neighbors xy

solve2 :: M.Map Point Int -> Int
solve2 g = product $ take 3 $ sortBy (flip compare) $ map length $ dff g' where
    (g', _, _) = graphFromEdges $ map getNode $ M.assocs g
    getNode (xy, d) = ((), xy, nbs) where
        nbs | d == 9    = []
            | otherwise = filter (maybe False (/=9) . (g M.!?)) $ neighbors xy

parse :: String -> M.Map Point Int
parse s = M.fromList $ zip ((,) <$> [1..n] <*> [1..m]) (concat xss) where
    xss = map (map digitToInt) $ lines s
    n = length xss
    m = length $ head xss

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
