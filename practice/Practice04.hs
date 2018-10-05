data Branch = Leaf Int | Fork Branch Branch
              deriving (Eq, Show)

br :: Branch
br = Fork (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) (Leaf 4)

extend :: Int -> Branch -> [Branch]
extend x t@(Leaf _) = [Fork (Leaf x) t]
extend x t@(Fork l r) =
  let bs = extend x l
      ns = map (\u -> Fork u r) bs
  in (Fork (Leaf x) t) : ns

brs1 :: [Int] -> [Branch]
bs1 [x] = [Leaf x]
brs1 (x:xs) = concatMap (extend x) (brs1 xs)
