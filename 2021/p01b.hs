countIncrease :: [Int] -> Int
countIncrease xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

threeMeasurement :: [Int] -> [Int]
threeMeasurement xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (drop 2 xs)

main :: IO ()
main = print . countIncrease . threeMeasurement . map read . lines =<< getContents
