data Op = Add | Sub | Mul | Div deriving (Show)
data Expr = Val Int | App Op Expr Expr deriving (Show)

valid :: Op -> Int -> Int -> Bool
valid Sub x y = x > y
valid Div x y = mod x y == 0
valid _ _ _ = True

apply :: Op -> Int -> Int -> Int
apply Add v1 v2 = v1+v2
apply Sub v1 v2 = v1-v2
apply Mul v1 v2 = v1*v2
apply Div v1 v2 = v1`div`v2

values :: Expr -> [Int]
values = undefined

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op left right) = [apply op x y | x <- eval left, y <- eval right, valid op x y]

subs :: [a] -> [[a]]
subs [x] = [[x]]
subs (x:xs) = xss ++ [x]: map(x:) xss
                  where xss = subs xs
subs [] = error "subs"

insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insert x) (perms xs)

choices :: [a] -> [[a]]
choices = concatMap perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

splits :: [a] -> [([a], [a])]
splits xs = [splitAt i xs | i <- [1..length xs - 1]]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- splits ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

solutions :: Int -> [Int] -> [Expr]
solutions n ns = [e | ns1 <- choices ns, e <- exprs ns1, eval e == [n]]
