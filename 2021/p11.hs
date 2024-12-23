import Control.Monad
import Control.Monad.Writer
import Data.Char
import Data.List
import qualified Data.Map as M

type Point = (Int, Int)
type Grid = M.Map Point Int

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

solve1 :: Grid -> Int
solve1 = getSum . execWriter . foldr (>=>) pure (replicate 100 $ flash . fmap (+1)) where
    flash :: Grid -> Writer (Sum Int) Grid
    flash g
        | M.null g9 = pure g
        | otherwise = do
            tell $ Sum (M.size g9)
            M.union (fmap (const 0) g9) <$> flash g''
      where
        (g9, g') = M.partition (>9) g
        g'' = foldr (M.adjust (+1)) g' $ concatMap neighbors $ M.keys g9

solve2 :: Grid -> Int
solve2 = length . unfoldr go where
    go g = ((), flash $ fmap (+1) g) <$ guard (any (/=0) $ M.elems g)
    flash g
        | M.null g9 = g
        | otherwise = M.union (fmap (const 0) g9) $ flash g''
      where
        (g9, g') = M.partition (>9) g
        g'' = foldr (M.adjust (+1)) g' $ concatMap neighbors $ M.keys g9

parse :: String -> Grid
parse = M.fromList . zip ((,) <$> [1..10] <*> [1..10]) . map digitToInt . filter isDigit

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
