{-# OPTIONS_GHC -Wall #-}
module Marhal03 where

data BinTree a = EmptyB
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a
               | Node2 (Tree23 a) a (Tree23 a)
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- порожнЇ 2-3-дерево!!!
                   deriving (Eq, Show)

-- Задача 1 -----------------------------------------
isSearch :: (Ord a) => BinTree a -> Bool
isSearch EmptyB = True
isSearch (Node x tl tr) = check LT x tl && check GT x tr
    where
        check _ _ EmptyB = True
        check comp x (Node y tl tr) = (comp == compare y x) && check LT y tl && check GT y tr

-- Задача 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch EmptyB _ = False
elemSearch (Node x tl tr) v = case compare v x of
    EQ -> True
    LT -> elemSearch tl v
    GT -> elemSearch tr v

-- Задача 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a
insSearch EmptyB v = Node v EmptyB EmptyB
insSearch bt@(Node x tl tr) v = case compare v x of
    EQ -> bt
    LT -> Node x (insSearch tl v) tr
    GT -> Node x tl (insSearch tr v)

-- Задача 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a
delSearch EmptyB v = EmptyB
delSearch (Node x tl tr) v = case compare v x of
    EQ -> concatB tl tr
    LT -> Node x (delSearch tl v) tr
    GT -> Node x tl (delSearch tr v)
    where
        concatB :: (Ord a) => BinTree a -> BinTree a -> BinTree a
        concatB EmptyB b = b
        concatB b EmptyB = b
        concatB (Node x tl tr) b2 = Node x tl (concatB tr b2)

-- Задача 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList  = toList . foldl insSearch EmptyB
    where
        toList EmptyB = []
        toList (Node x tl tr) = toList tl ++ x : toList tr
-- Задача 6-----------------------------------------
isTree23  :: (Ord a) => Tree23 a -> Bool
isTree23 Empty23 = True
isTree23 (Leaf x) = True
isTree23 (Node2 tl x tr) = x >= maxT tl && x == minT tr
isTree23 (Node3 tl x tm y tr) = x >= maxT tl && x == minT tm && y >= maxT tm && y == minT tr

maxT (Leaf x) = x
maxT (Node2 _ x Empty23) = x
maxT (Node2 _ _ tr) = maxT tr
maxT (Node3 _ _ _ x Empty23) = x
maxT (Node3 _ _ _ _ tr) = maxT tr

minT (Leaf x) = x
minT (Node2 Empty23 x _) = x
minT (Node2 tl _ _) = minT tl
minT (Node3 Empty23 x _ _ _) = x
minT (Node3 tl _ _ _ _) = minT tl

-- Задача 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Empty23 _ = False
elemTree23 (Leaf x) v = x == v
elemTree23 (Node2 tl x tr) v = case compare v x of
    EQ -> True
    LT -> elemTree23 tl v
    GT -> elemTree23 tr v
elemTree23 (Node3 tl x tm y tr) v = case (compare v x, compare v y) of
    (EQ, _) -> True
    (_, EQ) -> True
    (LT, _) -> elemTree23 tl v
    (_, LT) -> elemTree23 tm v
    (_, GT) -> elemTree23 tr v

--Задача 8-----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 a b = toList a == toList b
    where
        toList Empty23 = []
        toList (Leaf x) = [x]
        toList (Node2 tl x tr) = toList tl ++ toList tr
        toList (Node3 tl x tm y tr) = toList tl ++ toList tm ++ toList tr
-- «адача 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23  = undefined

-- isTerminal tree = True <=> €кщо сини вузла tree - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- –езультат вставки вузла в 2-3-дерево,
--   кор≥нь €кого - вузол вида Node2 або Node3 Ї об`Їкт ≥з (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значенн€ в b)
--  insert v tree - додаЇ значенн€ v в дов≥лье дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tree | isTerminal tree = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tree - додаЇтьс€ значенн€ v в дерево tree з конем - терм≥нальний вузол
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tree - додаЇ значенн€ v в дерево tree з корнем - нетерм≥нальний вузол
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined

---  Ѕ≥нарн≥ дерева
bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB)
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB)
                       EmptyB)

---- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1))
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5))
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8))
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )
