import Text.Printf

calculate :: String -> Double
calculate z= calcRPN $ shuntingYard z

calcRPN :: String -> Double
calcRPN expr = head $ (foldl interprete [] . words) $ expr where
  interprete s x
    | elem x ["+","-","*","/","^"] = calc x s
    | otherwise = read x:s
    where
      calc op (x:y:s) = case op of
        "^" -> y ** x:s
        "*" -> x * y:s
        "/" -> y / x:s
        "+" -> x + y:s
        "-" -> y - x:s

shuntingYard :: String -> String
shuntingYard expr = res where
        res = unwords $ reverse $ fst $ last (final ++ [lastStep])
        final = scanl f ([], []) $ words expr
        lastStep = (\(x,y) -> ((++) (reverse y) x, [])) $ last final
        f :: ([String], [String]) -> String -> ([String], [String])
        f (out, stack) t | isOperator t = ((++) (reverse $ takeWhile validOp stack) out, t:dropWhile validOp stack)
                         | t == "("  = (out, "(":stack)
                         | t == ")"  = ((++) (reverse (takeWhile (\e -> (/=) e "(") stack)) out, tail $ dropWhile (\e -> (/=) e "(") stack)
                         | otherwise = (t:out, stack) where
                             isOperator :: String -> Bool
                             isOperator (t:_) = elem t "-+/*^"
                             isOperator _      = False
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

main = do {
  a <- getLine;
  putStrLn $ shuntingYard $ a
}
