module Counting where

counting :: String -> String
counting line = let ls = lines line
                    ws = concatMap words ls
                    cc = sum $ map length ws
                in show (length ls, length ws, cc)
