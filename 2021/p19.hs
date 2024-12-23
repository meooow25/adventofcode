import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T

type Vec3 = (Int, Int, Int)

add :: Vec3 -> Vec3 -> Vec3
add (x, y, z) (x', y', z') = (x + x', y + y', z + z')

neg :: Vec3 -> Vec3
neg (x, y, z) = (-x, -y, -z)

sub :: Vec3 -> Vec3 -> Vec3
sub v v' = v `add` neg v'

magnitude :: Vec3 -> Int
magnitude (x, y, z) = abs x + abs y + abs z

orientations :: Vec3 -> [Vec3]
orientations = concatMap setyz . setx where
    setx  (x, y, z) = [(x, y, z), (-x, z, y), (y, z, x), (-y, x, z), (z, x, y), (-z, y, x)]
    setyz (x, y, z) = [(x, y, z), (x, -z, y), (x, -y, -z), (x, z, -y)]

reorientMany :: [Vec3] -> [[Vec3]]
reorientMany = transpose . map orientations

----------------------------------------

type Scanner = S.Set Vec3
type ScannerPos = Vec3
type Beacons = S.Set Vec3

tryMerge :: Scanner -> Scanner -> Maybe (ScannerPos, Scanner)
tryMerge s s' = listToMaybe $ do
    first      <- S.toList s
    reoriented <- reorientMany $ S.toList s'
    first'     <- reoriented
    let diff    = first `sub` first'
        shifted = S.fromList $ map (add diff) reoriented
    guard $ S.size (S.intersection s shifted) >= 12
    pure (neg diff, S.union s shifted)

solve :: [Scanner] -> (Beacons, [ScannerPos])
solve [] = error "no scanners"
solve (s:ss) = go s [(0, 0, 0)] ss where
    go s ps [] = (s, ps)
    go s ps ss = go s' ps' ss' where
        (s', ps', ss') = foldl' f (s, ps, []) ss
        f (s, ps, ss) s' = case tryMerge s s' of
            Nothing       -> (s,   ps,   s':ss)
            Just (p, s'') -> (s'', p:ps, ss)

parse :: String -> [Scanner]
parse = map parseScanner . splitScanners where
    splitScanners = map T.unpack . T.splitOn (T.pack "\n\n") . T.pack
    parseScanner = S.fromList . map (read . (\p -> "(" ++ p ++ ")")) . tail . lines

pairs :: [a] -> [(a, a)]
pairs xs = [(x, x') | (x:xs') <- tails xs, x' <- xs']

main :: IO ()
main = do
    p <- parse <$> getContents
    let (beacons, scanners) = solve p
    print $ S.size beacons
    print $ maximum $ map (magnitude . uncurry sub) $ pairs scanners
