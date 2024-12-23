import Data.Char
import Data.List
import Data.Maybe

segs :: [[Int]]
segs = map (map digitToInt)
    [ "012456"
    , "25"
    , "02346"
    , "02356"
    , "1235"
    , "01356"
    , "013456"
    , "025"
    , "0123456"
    , "012356"
    ]

allPerms :: [Int -> Int]
allPerms = map (!!) (permutations [0..7])

decode :: [String] -> (Int -> Int) -> Maybe [Int]
decode digits perm = mapM ((`elemIndex` segs) . sort . map (perm . subtract (ord 'a') . ord)) digits

solve1 :: [([String], [String])] -> Int
solve1 = sum . map solve1 where
    solve1 (_, digits) = length $ filter ((`elem` uniqueLengths) . length) digits
    uniqueLengths = [2, 4, 3, 7]

solve2 :: [([String], [String])] -> Int
solve2 = sum . map solve1 where
    solve1 (patterns, digits) =
        read . concatMap show .
        fromJust . decode digits .
        head . filter (maybe False ((==[0..9]) . sort) . decode patterns) $ allPerms

parse :: String -> [([String], [String])]
parse = map parseLine . lines where
    parseLine s = let (s', s'') = break (=='|') s in (words s', words $ tail s'')

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
