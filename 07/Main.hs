howMuchFuel :: [Int] -> Int
howMuchFuel xs = minimum $ map (`spentFuel` xs) range
    where range = [minimum xs .. maximum xs]

getDistances :: Int -> [Int] -> [Int]
getDistances n = map (abs . subtract n)

spentFuel :: Int -> [Int] -> Int
spentFuel n xs = sum $ getDistances n xs

splitBy :: Char -> String -> [String]
splitBy del xs = splitBy' del xs []

splitBy' :: Char -> String -> [Char] -> [String]
splitBy' _ [] acc = [reverse acc]
splitBy' del (x:xs) acc
    | x == del = reverse acc : splitBy' del xs []
    | otherwise = splitBy' del xs (x : acc)

getCrabPositions :: String -> [Int]
getCrabPositions input = map read $ splitBy ',' input

main :: IO ()
main = do
    contents <- readFile "input"
    let input = getCrabPositions contents
    print input
    print (howMuchFuel input)