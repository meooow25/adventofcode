import Data.Char
import Data.List
import Data.Bits

solve :: [String] -> Int
solve xs = sum gam * sum eps where
    len = length $ head xs
    xs' = map parseBin xs
    go p = if length o > length z then (p, 0) else (0, p) where
        (o, z) = partition ((==p) . (.&.p)) xs'
    (gam, eps) = unzip $ map go $ take len $ iterate (*2) 1

parseBin :: String -> Int
parseBin = foldl' (\acc c -> acc * 2 + digitToInt c) 0

main :: IO ()
main = print . solve . lines =<< getContents
