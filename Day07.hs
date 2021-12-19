import Text

howMuchFuel :: [Int] -> Int
howMuchFuel xs = minimum $ map (`spentFuel` xs) range
    where range = [minimum xs .. maximum xs]

getDistances :: Int -> [Int] -> [Int]
getDistances n = map (abs . subtract n)

spentFuel :: Int -> [Int] -> Int
spentFuel n xs = sum $ getDistances n xs

getCrabPositions :: String -> [Int]
getCrabPositions input = map read $ splitBy ',' input

main :: IO ()
main = do
    contents <- readFile "inputs/Day07"
    let input = getCrabPositions contents
    print input
    print (howMuchFuel input)