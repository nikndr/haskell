allPrefixs :: [Int] -> [[Int]]
allPrefixs xs = [take x xs | x <- [1..length xs]]

diff :: [Int] -> [Int] -> [Int]
diff xs ys = filter (notInList ys) xs

notInList :: [Int] -> Int -> Bool
notInList xs x = notElem x xs
