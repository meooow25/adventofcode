import Data.Char
import Data.List

solve :: [String] -> Int
solve xs = parseBin (go oxytakeo) * parseBin (go co2takeo) where
    go f = go' 0 xs where
        go' _ []  = error "!"
        go' _ [x] = x
        go' i xs  = go' (i + 1) xs' where
            (o, z) = partition ((=='1') . (!!i)) xs
            xs' = if length o `f` length z then o else z
    oxytakeo = (>=)
    co2takeo = (<)

parseBin :: String -> Int
parseBin = foldl' (\acc c -> acc * 2 + digitToInt c) 0

main :: IO ()
main = print . solve . lines =<< getContents
