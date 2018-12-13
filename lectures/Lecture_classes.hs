class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)

instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _ == _ = False

data Ordering = LT | EQ | GT

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>=), (>) :: a -> a -> Bool
  min, max :: a -> a -> a
  compare x y = if x == y then EQ else if x <= y then LT else GT
  x < y  = compare x y == LT
  x <= y = compare x /= GT
  min x y = if x <= y then x else y
  -- ...
