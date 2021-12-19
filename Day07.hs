import Text

howMuchFuel :: (Int -> Int -> Int) -> [Int] -> Int -> Int
howMuchFuel fuelFunc xs n = sum $ map (fuelFunc n) xs

howMuchWithCheapFuel :: [Int] -> Int
howMuchWithCheapFuel xs = minimum $ map (howMuchFuel cost xs) range
    where   range = [minimum xs .. maximum xs]
            cost a b = abs (a - b)

howMuchWithPriceyFuel :: [Int] -> Int
howMuchWithPriceyFuel xs = minimum $ map (howMuchFuel cost xs) range
    where   range = [minimum xs .. maximum xs]
            cost a b = sum [1 .. (abs (a - b))]

getCrabPositions :: String -> [Int]
getCrabPositions input = map read $ splitBy ',' input

main :: IO ()
main = do
    contents <- readFile "inputs/Day07"
    let input = getCrabPositions contents
    print (howMuchWithCheapFuel input)
    print (howMuchWithPriceyFuel input)