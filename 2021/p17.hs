import Control.Monad
import Data.Char

-- 0 < x1 <= x2; y1 <= y2 < 0

solve1 :: (Int, Int, Int, Int) -> Int
solve1 (_, _, y1, _) = y1 * (y1 + 1) `div` 2 -- assume that this will work for some vx 🙃

solve2 :: (Int, Int, Int, Int) -> Int
solve2 (x1, x2, y1, y2) = length $ do
    vx <- [1..x2]
    vy <- [y1.. -y1]
    guard $ not . null $
        dropWhile tooClose $ takeWhile (not . tooFar) $ zip (positions ax vx) (positions ay vy)
  where
    positions a v = scanl (+) 0 $ iterate a v
    ax = max 0 . subtract 1
    ay = subtract 1
    tooFar   (x, y) = x > x2 || y < y1
    tooClose (x, y) = x < x1 || y > y2

parse :: String -> (Int, Int, Int, Int)
parse s = (x1, x2, y1, y2) where
    [x1, x2, y1, y2] = map read $ words $ map (\c -> if c == '-' || isDigit c then c else ' ') s

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
