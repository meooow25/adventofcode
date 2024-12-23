import qualified Data.Map as M

type Point    = (Int, Int)
data Cucumber = DownC | RightC deriving Eq
data Grid     = Grid Point (M.Map Point Cucumber)

down, right :: Point -> Point -> Point
down  (n, _) (x, y) = ((x + 1) `mod` n, y)
right (_, m) (x, y) = (x, (y + 1) `mod` m)

solve :: Grid -> Int
solve (Grid nm g) = 1 + length (takeWhile id $ zipWith (/=) gs (tail gs)) where
    gs = iterate (step DownC down . step RightC right) g
    step ctyp nxt g = M.fromList $ do
        (xy, c) <- M.assocs g
        let xy' = nxt nm xy
        pure $ if c == ctyp && xy' `M.notMember` g
            then (xy', c)
            else (xy,  c)

parse :: String -> Grid
parse s = Grid (n, m) g where
    ls = lines s
    n = length ls
    m = length $ head ls
    g = M.fromList
        [ ((x, y), c')
        | (x, l) <- zip [0..] ls
        , (y, c) <- zip [0..] l
        , c /= '.'
        , let c' = case c of
                '>' -> RightC
                'v' -> DownC
                _   -> error "bad cucumber"
        ]

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve p
