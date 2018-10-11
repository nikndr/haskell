{-# OPTIONS_GHC -Wall #-}
module HWP05 where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

-- Задача 1 -----------------------------------------
-- instance Ord AbstractInteger where
   -- (<=) :: (Ord i) => i -> i -> Bool
   -- (<=) = undefined

-- Задача 2 ---------------------------------------- ???????????????????
aiToInteger :: AbstractInteger -> Integer
aiToInteger (Zero) = 0
aiToInteger (Succ x) = 1 + aiToInteger x
aiToInteger (Pred x) = aiToInteger x - 1

-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs (Zero) (Zero) = 0
plusAbs (Zero) x = x
plusAbs x (Zero) = x
plusAbs (Succ x) (Succ y) = Succ (Succ (plusAbs x y))
plusAbs (Succ x) (Pred y) = Succ (Pred (plusAbs x y))
plusAbs (Pred x) (Succ y) = Pred (Succ (plusAbs x y))
plusAbs (Pred x) (Pred y) = Pred (Pred (plusAbs x y))

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs = undefined

-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate      = undefined
    fromInteger = undefined
    abs         = undefined
    signum      = undefined

-- Задача 6 -----------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial = undefined

-- Задача  7 -----------------------------------------
data Quaternion = Quaternion Double Double Double Double deriving (Eq)

instance Show Quaternion where
    show = undefined

-- Задача 8 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion = undefined

-- Задача 9 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion = undefined

--- Задача 10 ----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    negate      = undefined
    fromInteger = undefined
    abs         = undefined
    signum      = undefined
