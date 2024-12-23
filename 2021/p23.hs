import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

data Amphipod = A | B | C | D deriving (Eq, Ord, Read)
data Burrow   = B1 | B2 | B3 | B4 deriving (Enum, Eq, Ord)
data Space    = S1 | S2 | S4 | S6 | S8 | S10 | S11 deriving (Enum, Eq, Ord)

cost :: Amphipod -> Int
cost A = 1
cost B = 10
cost C = 100
cost D = 1000

goal :: Amphipod -> Burrow
goal A = B1
goal B = B2
goal C = B3
goal D = B4

type Dist = Int

getOut :: Burrow -> ([(Space, Dist)], [(Space, Dist)])
getOut = first reverse . go where
    go B1 = splitAt 2 $ zip hallway [3, 2, 2, 4, 6, 8, 9]
    go B2 = splitAt 3 $ zip hallway [5, 4, 2, 2, 4, 6, 7]
    go B3 = splitAt 4 $ zip hallway [7, 6, 4, 2, 2, 4, 5]
    go B4 = splitAt 5 $ zip hallway [9, 8, 6, 4, 2, 2, 3]
    hallway = [S1 .. S11]

getOutOk :: [Space] -> Burrow -> [(Space, Dist)]
getOutOk occ b = p l ++ p r where
    (l, r) = getOut b
    p = takeWhile ((`notElem` occ) . fst)

data State = State
           { burrows_ :: M.Map Burrow [Amphipod]
           , spaces_  :: M.Map Space Amphipod
           } deriving (Eq, Ord)

next :: State -> [(Int, State)]
next (State burrows spaces) = burrowToSpace ++ spaceToBurrow where
    total = (length spaces + sum (fmap length burrows)) `div` 4
    occ = M.keys spaces
    burrowToSpace = do
        (b, as@(a:as')) <- M.assocs burrows
        (s, d) <- getOutOk occ b
        let d'  = d + total - length as
            st' = State (M.insert b as' burrows) (M.insert s a spaces)
        pure (cost a * d', st')
    spaceToBurrow = do
        (s, a) <- M.assocs spaces
        let b  = goal a
            as = burrows M.! b
        guard $ all (==a) as
        (s', d) <- getOutOk (delete s occ) b
        guard $ s == s'
        let d'  = d + total - length as - 1
            st' = State (M.insert b (a:as) burrows) (M.delete s spaces)
        pure (cost a * d', st')

shortestPath :: (Ord v) => v -> (v -> [(Int, v)]) -> v -> Int
shortestPath src nxt dst = dijkstra (M.singleton src 0, S.singleton (0, src)) where
    dijkstra (dist, pq) = case S.minView pq of
        Nothing -> error "dst not found"
        Just ((d, u), _) | u == dst -> d
        Just ((d, u), pq') -> dijkstra $ foldl' upd (dist, pq') $ nxt u where
            upd dp@(dist, pq) (w, v) = case dist M.!? v of
                Nothing            -> (dist', pq')
                Just dv | dv' < dv -> (dist', S.delete (dv, v) pq')
                _                  -> dp
              where
                dv'   = d + w
                dist' = M.insert v dv' dist
                pq'   = S.insert (dv', v) pq

solve1 :: M.Map Burrow [Amphipod] -> Int
solve1 b = shortestPath st next st' where
    st  = State b M.empty
    st' = State (M.fromList $ map gen [A, B, C, D]) M.empty where gen a = (goal a, [a, a])

solve2 :: M.Map Burrow [Amphipod] -> Int
solve2 b = shortestPath st next st' where
    b' = M.fromList
        [ (b, [a0,a1,a2,a3])
        | ((b, [a0,a3]), [a1,a2]) <- zip (M.assocs b) [[D,D], [C,B], [B,A], [A,C]]
        ]
    st  = State b' M.empty
    st' = State (M.fromList $ map gen [A, B, C, D]) M.empty where gen a = (goal a, [a, a, a, a])

parse :: String -> M.Map Burrow [Amphipod]
parse s = M.fromList $ zip [B1 .. B4] $ zipWith (\a b -> [a, b]) row1 row2 where
    (row1, row2) = splitAt 4 $ map (read . (:[])) (filter isAlpha s)

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
