import System.IO

countIncreased :: [Int] -> Int
countIncreased [] = 0
countIncreased (x:y:xs)
    | y > x = 1 + countIncreased (y:xs)
    | otherwise = 0 + countIncreased (y:xs)
countIncreased (x:xs) = 0

sumOfWindows :: [Int] -> [Int]
sumOfWindows [] = []
sumOfWindows xs = sum (take 3 xs) : sumOfWindows (tail xs)

main :: IO ()
main = do
        contents <- readFile "inputs/Day01"
        let lines = words contents
        let list = map read lines
        print (countIncreased list)
        print (countIncreased $ sumOfWindows list)
