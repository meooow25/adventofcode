import Data.Char
import Data.List
import Data.Maybe

type Range = (Int, Int)
type RectN = [Range] -- n-element list for n dimensions
data Instruction = Inst { on :: Bool, bnds :: RectN }

size :: RectN -> Int
size = product . map (\(l, r) -> r - l + 1)

split :: Range -> Range -> (Maybe Range, [Range])
split (a1, a2) (b1, b2)
    | c1 > c2   = (Nothing,       [(b1, b2)])
    | otherwise = (Just (c1, c2), [(c1, c2)] ++ [(b1, c1 - 1) | b1 < c1] ++ [(c2 + 1, b2) | c2 < b2])
  where
    c1 = max a1 b1
    c2 = min a2 b2

splitN :: RectN -> RectN -> (Maybe RectN, [RectN])
splitN a b = (sequence is, sequence ps) where
    (is, ps) = unzip $ zipWith split a b

cutN :: RectN -> RectN -> [RectN]
cutN a b = if isJust isect then tail pieces else [b] where
    (isect, pieces) = splitN a b

solve :: [Instruction] -> [RectN]
solve = foldl' apply [] where
    apply ons inst = [bnds inst | on inst] ++ concatMap (cutN $ bnds inst) ons

parse :: String -> [Instruction]
parse = map parseLine . lines where
    parseLine s = Inst o [(x1, x2), (y1, y2), (z1, z2)] where
        o = head (words s) == "on"
        [x1, x2, y1, y2, z1, z2] = map read $ words $ map (\c -> if c == '-' || isDigit c then c else ' ') s

main :: IO ()
main = do
    p <- parse <$> getContents
    let boxes = solve p
        chopTo50 = fst . splitN (replicate 3 (-50, 50))
    print $ sum $ map size $ mapMaybe chopTo50 boxes
    print $ sum $ map size boxes
