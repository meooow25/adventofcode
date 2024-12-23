import Data.Ix
import Data.List
import qualified Data.Set as S

type Point = (Int, Int)
data Image = Image { bounds_ :: (Point, Point), on_ :: S.Set Point, onOutside_ :: Bool }

neighborhood :: Point -> [Point]
neighborhood (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]

expandBy1 :: (Point, Point) -> (Point, Point)
expandBy1 ((x1, y1), (x2, y2)) = ((x1-1, y1-1), (x2+1, y2+1))

enhance :: (Int -> Bool) -> Image -> Image
enhance setOn (Image bnds on onOutside) = Image bnds' on' onOutside' where
    bnds' = expandBy1 bnds
    on' = S.fromList $ filter setPosOn $ range bnds'
    setPosOn = setOn . toInt . map posOn . neighborhood
    posOn p = S.member p on || not (inRange bnds p) && onOutside
    toInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0
    onOutside' = onOutside && setOn 0x1ff || not onOutside && setOn 0

countOn :: Image -> Int
countOn (Image _ _  True) = error "infinite"
countOn (Image _ on _)    = S.size on

solve :: (Int -> Bool, Image) -> Int -> Int
solve (setOn, im) n = countOn $ foldl' (flip ($)) im $ replicate n $ enhance setOn

parse :: String -> (Int -> Bool, Image)
parse s = (flip S.member setOn, Image ((1, 1), (n, m)) on False) where
    (h:"":s') = lines s
    setOn = S.fromList [i | (i, c) <- zip [0 :: Int ..] h, c == '#']
    n = length s'
    m = length $ head s'
    on = S.fromList [(x, y) | (x, l) <- zip [1..] s', (y, c) <- zip [1..] l, c == '#']

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve p 2
    print $ solve p 50
