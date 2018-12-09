import Text.Printf

prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2

leftAssoc "^" = False
leftAssoc _ = True

isOp (t:[]) = t `elem` "-+/*^"
isOp _      = False

simSYA xs = res where
      res = unwords $ reverse $ fst $ getTwo $ last ((++) final [lastStep])
      getTwo (f,s,_) = (f,s)
      final = scanl f ([],[],"") $ words xs
      lastStep = (\(x,y,_) -> (reverse y ++ x, [], "")) $ last final
      f (out,st,_) t | isOp t    =
                       (reverse (takeWhile testOp st) ++ out
                       , (t:) $ (dropWhile testOp st), t)
                     | t == "("  = (out, "(":st, t)
                     | t == ")"  = (reverse (takeWhile (/="(") st) ++ out,
                                   tail $ dropWhile (/="(") st, t)
                     | otherwise = (t:out, st, t)
        where testOp x = isOp x && (leftAssoc t && prec t == prec x
                                    || prec t < prec x)

-- main = do
--     a <- getLine
--     printf "%30s%20s%7s" "Output" "Stack" "Token"
--     mapM_ (\(x,y,z) -> printf "%30s%20s%7s\n"
--             (unwords $ reverse x) (unwords y) z) $ simSYA $ words a
