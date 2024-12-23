import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Text.Printf

data Packet = Literal  { pVersion :: Int, pValue :: Int }
            | Operator { pVersion :: Int, pType :: Int, pPackets :: [Packet] }

solve1 :: Packet -> Int
solve1 = go where
    go (Literal  ver _)    = ver
    go (Operator ver _ ps) = ver + sum (map go ps)

solve2 :: Packet -> Int
solve2 = go where
    go (Literal  _ val)    = val
    go (Operator _ typ ps) = ops !! typ $ map go ps
    ops = [sum, product, minimum, maximum, error "literal"] ++
          map (\f [a, b] -> fromEnum $ f a b) [(>), (<), (==)]

parse :: String -> Packet
parse = fst . head . readP_to_S packet . concatMap (printf "%04b" . digitToInt) where
    packet = literal +++ operator
    literal = Literal <$> readInt 3 <* string "100" <*> value where
        value = merge <$> many (char '1' *> readString 4) <* char '0' <*> readString 4
        merge init_ last_ = parseBin $ concat init_ ++ last_
    operator = Operator <$> readInt 3 <*> readInt 3 <*> packets where
        packets = (char '0' *> byLength) +++ (char '1' *> byCount)
        byLength = fst . last . readP_to_S (many packet) <$> (readString =<< readInt 15)
        byCount = flip count packet =<< readInt 11

    readString n = count n $ char '0' +++ char '1'
    readInt = fmap parseBin . readString

parseBin :: String -> Int
parseBin = foldl' (\acc c -> acc * 2 + digitToInt c) 0

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve1 p
    print $ solve2 p
