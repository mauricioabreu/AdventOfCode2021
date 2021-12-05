import Data.List
import Data.Char (digitToInt)

mostCommon :: [Int] -> Int
mostCommon xs = 
    let z = length $ filter (== 0) xs
        o = length xs - z
    in if z > o then 0 else 1

invert :: [Int] -> [Int]
invert = map invert'
    where   invert' 0 = 1
            invert' _ = 0

toDecimal :: [Int] -> Int
toDecimal = foldl (\acc x -> acc * 2 + x) 0

powerRates :: [[Int]] -> ([Int], [Int])
powerRates b =
    let v = map mostCommon $ transpose b
        gr = v
        er = invert v
    in (gr, er)

calcRates :: [String] -> Int
calcRates i = toDecimal mc * toDecimal lc
    where   ls = [map digitToInt l | l <- i]
            pr = powerRates ls
            mc = fst pr
            lc = snd pr

main :: IO ()
main = do
    contents <- readFile "input"
    print (calcRates (lines contents))