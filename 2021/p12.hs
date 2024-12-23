import Control.Monad.Writer
import Data.Char
import qualified Data.Map as M

type Node = String
type Graph = M.Map Node [Node]

small :: Node -> Bool
small = all isLower

solve1 :: Graph -> Int
solve1 g = getSum $ execWriter $ dfs [] "start" where
    dfs :: [Node] -> Node -> Writer (Sum Int) ()
    dfs _ "end" = tell $ Sum 1
    dfs seen u | u `elem` seen = pure ()
    dfs seen u = mapM_ (dfs seen') $ g M.! u where
        seen' = if small u then u:seen else seen

solve2 :: Graph -> Int
solve2 g = getSum $ execWriter $ dfs [] False "start" where
    dfs :: [Node] -> Bool -> Node -> Writer (Sum Int) ()
    dfs _ _ "end" = tell $ Sum 1
    dfs seen twice u | u `elem` seen && (u == "start" || twice) = pure ()
    dfs seen twice u = mapM_ (dfs seen' twice') $ g M.! u where
        seen' = if small u then u:seen else seen
        twice' = twice || u `elem` seen

parse :: String -> Graph
parse = M.fromListWith (++) . concatMap parseEdge . lines where
    parseEdge s = [(u, [v]), (v, [u])] where
        [u, v] = words $ map (\c -> if c == '-' then ' ' else c) s

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
