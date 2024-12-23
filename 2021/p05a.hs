import qualified Data.Map as M

type Point = (Int, Int)
type Line = (Point, Point)

axisAligned :: Line -> Bool
axisAligned ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

genPoints :: Line -> [Point]
genPoints ((x1, y1), (x2, y2)) =
    [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

solve :: [Line] -> Int
solve ls = M.size $ M.filter (>1) $ M.fromListWith (+) [(p, 1) | p <- concatMap genPoints ls'] where
    ls' = filter axisAligned ls

parse :: String -> [Line]
parse = map parseLine . lines where
    parseLine l = (p1, p2) where
        [p1s, "->", p2s] = words l
        [p1, p2] = map parsePoint [p1s, p2s]
    parsePoint ps = read $ "(" ++ ps ++ ")"

main :: IO ()
main = print . solve . parse =<< getContents
