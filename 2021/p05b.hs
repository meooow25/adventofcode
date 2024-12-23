import qualified Data.Map as M

type Vec2 = (Int, Int)
data Line = Line { start :: Vec2, dir :: Vec2, len :: Int }

add, sub :: Vec2 -> Vec2 -> Vec2
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

mkLine :: Vec2 -> Vec2 -> Line
mkLine p1 p2
    | ok        = Line p1 (signum dx, signum dy) n
    | otherwise = error "bad line"
  where
    (dx, dy) = p2 `sub` p1
    ok = dx == 0 || dy == 0 || abs dx == abs dy
    n = abs dx `max` abs dy

genPoints :: Line -> [Vec2]
genPoints (Line p d n) = take (n + 1) $ iterate (add d) p

solve :: [Line] -> Int
solve ls = M.size $ M.filter (>1) $ M.fromListWith (+) [(p, 1) | p <- concatMap genPoints ls]

parse :: String -> [Line]
parse = map parseLine . lines where
    parseLine l = mkLine p1 p2 where
        [p1s, "->", p2s] = words l
        [p1, p2] = map parsePoint [p1s, p2s]
    parsePoint ps = read $ "(" ++ ps ++ ")"

main :: IO ()
main = print . solve . parse =<< getContents
