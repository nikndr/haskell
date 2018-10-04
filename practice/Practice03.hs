data Branch = Leaf Int | Fork Branch Branch
              deriving (Eq, Show)

br :: Branch
br = Fork (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) (Leaf 4)

frigle :: Branch -> [Int]
frigle (Leaf v) = [v]
frigle (Fork l r) = frigle l ++ frigle r

height :: Branch -> Int
height (Leaf v) = 0
height (Fork l r) = 1 + max (height l) (height r)

showB :: Branch -> String
showB (Leaf v) = show v
showB (Fork l r) = "(" ++ show l ++ " " ++ show r ++ ")" 

brs :: [Int] -> [Branch]
brs [v] = [Leaf v]
brs xs = [Fork lt rt | i <- [1..length xs - 1], lt <- brs (take i xs), rt <- brs (drop i xs)]

takeMinBr :: [Branch] -> [Branch]
takeMinBr bs = let h = minimum (map height bs)
               in filter ((== h) . height) bs

minBr xs = takeMinBr (brs xs)
