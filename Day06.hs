{-# LANGUAGE TupleSections #-}

import qualified Data.Map as Map
import Text

toMap :: (Ord k, Num a) => [k] -> Map.Map k a
toMap xs = Map.fromListWith (+) (map (, 1) xs)

howManyLanternfish :: [Int] -> Int -> Int
howManyLanternfish xs t = sum $ iterate growUp mlf !! t 
    where mlf = toMap xs

growUp :: Map.Map Int Int -> Map.Map Int Int
growUp lf = Map.fromListWith (+) $ do
    (l, n) <- Map.toList lf
    if l == 0
        then [(6, n), (8, n)]
        else [(l - 1, n)]

gatherLanternfishes :: String -> [Int]
gatherLanternfishes input = map read $ splitBy ',' input

main :: IO ()
main = do
    contents <- readFile "inputs/Day06"
    let input = gatherLanternfishes contents
    print (howManyLanternfish input 80)
    print (howManyLanternfish input 256)