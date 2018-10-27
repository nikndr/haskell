putN :: Int -> String -> IO()
putN n ln = if n <= 1 then putStrLn ln else
            do putStrLn ln
               putN (n-1) ln

sum2 :: IO()
sum2 = do a <- getLine
          b <- getLine
          putStrLn $ evalSum a b

evalSum :: String -> String -> String
evalSum a b = case (takeInt a, takeInt b) of
                   (Just x, Just y) -> show(x+y)
                   (_,_)            -> "error"

takeInt :: String -> Maybe Int
takeInt sm = let (_, sm1) = span (==' ') sm
                 (num, sm2) = span (`elem` "1234567890") sm1
                 (_, sm3) = span (==' ') sm2
                 in if null sm3 then Just (read num) else Nothing

firstN :: IO ()
firstN = do putStr "file>"
            fname <- getLine
            putStr "N>"
            numLn <- getLine
            file  <- readFile fname
            putStr (takeN file numLn)

takeN :: String -> String -> String
takeN file ns = unlines $ take (read ns) (lines file)
