import Control.Monad
import Data.List
import qualified Data.Map.Strict as M

data ALUState = ALUState
              { aw :: {-# UNPACK #-} !Int
              , ax :: {-# UNPACK #-} !Int
              , ay :: {-# UNPACK #-} !Int
              , az :: {-# UNPACK #-} !Int
              } deriving (Eq, Ord, Show)
type Step = Int -> ALUState -> ALUState

variables :: String
variables = "wxyz"

readA :: Char -> ALUState -> Int
readA 'w' = aw
readA 'x' = ax
readA 'y' = ay
readA 'z' = az
readA _   = error "bad variable"

writeA :: Int -> Char -> ALUState -> ALUState
writeA v 'w' alu = alu { aw = v }
writeA v 'x' alu = alu { ax = v }
writeA v 'y' alu = alu { ay = v }
writeA v 'z' alu = alu { az = v }
writeA _   _ _ = error "bad variable"

modifyA :: (Int -> Int) -> Char -> ALUState -> ALUState
modifyA f c alu = writeA (f (readA c alu)) c alu

solve :: [Step] -> (Int -> Int -> Int) -> Int
solve steps choose = go steps $ M.singleton start 0 where
    start = ALUState 0 0 0 0
    go [] _ = error "no steps"
    go [s] m = foldl1' choose $ do
        (st, r) <- M.assocs m
        n       <- [1..9]
        let st' = s n st
            r'  = r * 10 + n
        guard $ az st' == 0
        pure r'
    go (s:ss) m = go ss $ M.fromListWith choose $ do
        (st, r) <- M.assocs m
        n       <- [1..9]
        let st' = s n st
            r'  = r * 10 + n
        -- hack assuming a large value will not reach 0, speeds things up
        -- guard $ az st' < 1000000
        pure (st', r')

parse :: String -> [Step]
parse = go . lines where
    go [] = []
    go (l:ls) = (\v alu -> foldl' (flip ($)) (inp v alu) ops) : go ls'' where
        (ls', ls'') = break ("inp" `isPrefixOf`) ls
        inp = parseInp l
        ops = map parseOp ls'
    parseInp s = case words s of
        ["inp", a:_] -> \v -> writeA v a
        _            -> error "not inp"
    parseOp s = case words s of
        [op, a:_, b@(b':_)] -> case op of
            "add" -> makeOp (+)
            "mul" -> makeOp (*)
            "div" -> makeOp div
            "mod" -> makeOp mod
            "eql" -> makeOp $ \p q -> fromEnum $ p == q
            _     -> error "not op"
          where
            bv = read b
            makeOp f | b' `elem` variables = \m -> modifyA (`f` readA b' m) a m
                     | otherwise           = modifyA (`f` bv) a
        _ -> error "not op"

main :: IO ()
main = do
    p <- parse <$> getContents
    print $ solve p max
    print $ solve p min
