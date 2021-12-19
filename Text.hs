module Text where

splitBy :: Char -> String -> [String]
splitBy del xs = splitBy' del xs []

splitBy' :: Char -> String -> [Char] -> [String]
splitBy' _ [] acc = [reverse acc]
splitBy' del (x:xs) acc
    | x == del = reverse acc : splitBy' del xs []
    | otherwise = splitBy' del xs (x : acc)