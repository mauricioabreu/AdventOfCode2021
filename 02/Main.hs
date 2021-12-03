data Command = Forward Int | Up Int | Down Int

runInstructions :: [Command] -> Int -> Int -> Int
runInstructions [] h d = h * d
runInstructions (x:xs) h d = case x of
    Forward x -> runInstructions xs (h + x) d
    Up x -> runInstructions xs h (d - x)
    Down x -> runInstructions xs h (d + x)


runInstructionsWithAim :: [Command] -> Int -> Int -> Int -> Int
runInstructionsWithAim [] h d _ = h * d
runInstructionsWithAim (x:xs) h d a = case x of
    Forward x -> runInstructionsWithAim xs (h + x) (d + (a * x)) a
    Up x -> runInstructionsWithAim xs h d (a - x)
    Down x -> runInstructionsWithAim xs h d (a + x)


parse :: String -> Command
parse s = case words s of
    ["forward", x] -> Forward $ read x
    ["up", x] -> Up $ read x
    ["down", x] -> Down $ read x
    _ -> error "wrong command"


main :: IO ()
main = do
        contents <- readFile "input"
        print (runInstructions (map parse (lines contents)) 0 0)
        print (runInstructionsWithAim (map parse (lines contents)) 0 0 0)
