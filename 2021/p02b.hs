import Data.Bifunctor

parseLine :: String -> ((Int, Int), Int) -> ((Int, Int), Int)
parseLine s = case dir of
    "forward" -> \((x, y), a) -> ((x + delta, y + a * delta), a)
    "down"    -> second (+delta)
    "up"      -> second (+ (-delta))
    _         -> error "!"
  where
    [dir, delta'] = words s
    delta = read delta'

main :: IO ()
main = print . uncurry (*) . fst . ($ ((0, 0), 0)) . foldr (flip (.)) id . map parseLine . lines =<< getContents
