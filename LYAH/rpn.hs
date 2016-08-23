import System.Environment

main = do
    (expression:_) <- getArgs
    putStrLn $ show $ solveRPN expression

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
                where
                    foldingFunction (x:y:ys) "+" = (y + x):ys
                    foldingFunction (x:y:ys) "-" = (y - x):ys
                    foldingFunction (x:y:ys) "*" = (y * x):ys
                    foldingFunction (x:y:ys) "/" = (y / x):ys
                    foldingFunction (x:y:ys) "^" = (y ** x):ys
                    foldingFunction (x:ys) "ln" = log x:ys
                    foldingFunction ys "sum" = [sum ys]
                    foldingFunction xs numberString = read numberString:xs

