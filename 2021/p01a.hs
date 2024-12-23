countIncrease :: [Int] -> Int
countIncrease xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

main :: IO ()
main = print . countIncrease . map read . lines =<< getContents
