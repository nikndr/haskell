import Text.Printf

calculate :: String -> Double
calculate z = calcRPN $ shuntingYard z

calcRPN :: String -> Double
calcRPN expr = head $ (foldl eval [] . words) $ expr where
     eval (x:y:ys) "*" = ((*) x y):ys
     eval (x:y:ys) "+" = ((+) x y):ys
     eval (x:y:ys) "-" = ((-) y x):ys
     eval (x:y:ys) "/" = ((/) y x):ys
     eval (x:y:ys) "^" = ((**) y x):ys
     eval (x:xs) "log" = log x:xs
     eval (x:xs) "sin" = sin x:xs
     eval (x:xs) "cos" = cos x:xs
     eval xs numberString = read numberString:xs


shuntingYard :: String -> String
shuntingYard expr = res where
        res = unwords $ reverse $ fst $ last ((++) final [lastStep])
        final = scanl f ([], []) $ words expr
        lastStep = (\(x,y) -> ((++) (reverse y) x, [])) $ last final

        f :: ([String], [String]) -> String -> ([String], [String])
        f (out, stack) t | isOperator t = ((++) (reverse $ takeWhile validOp stack) out, t:dropWhile validOp stack)
                         | (==) t "("  = (out, "(":stack)
                         | (==) t ")"  = ((++) (reverse (takeWhile (\e -> (/=) e "(") stack)) out, tail $ dropWhile (\e -> (/=) e "(") stack)
                         | otherwise = (t:out, stack) where

                             isOperator :: String -> Bool
                             isOperator t = elem t ["^","*","/","+","-","sin","cos","log"]

                             leftAssoc :: String -> Bool
                             leftAssoc "^" = False
                             leftAssoc _ = True

                             validOp :: String -> Bool
                             validOp x = isOperator x && (leftAssoc t && prec t == prec x || prec t < prec x) where

                                 prec :: String -> Int
                                 prec "^" = 4
                                 prec "*" = 3
                                 prec "/" = 3
                                 prec "+" = 2
                                 prec "-" = 2
                                 prec _ = 5

main = do {
  a <- getLine;
  putStrLn $ shuntingYard $ a;
}
