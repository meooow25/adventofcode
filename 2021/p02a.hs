import Data.Bifunctor

parseLine :: String -> (Int, Int) -> (Int, Int)
parseLine s = case dir of
    "forward" -> first  (+delta)
    "down"    -> second (+delta)
    "up"      -> second (+ (-delta))
    _         -> error "!"
  where
    [dir, delta'] = words s
    delta = read delta'

main :: IO ()
main = print . uncurry (*) . ($ (0, 0)) . foldr (.) id . map parseLine . lines =<< getContents
