{-# OPTIONS_GHC -Wall #-}
module Marhal05 where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Eq, Show)


-- instance Show AbstractInteger where
--   show ai
--        | int > 0 = "Succ " ++ show int
--        | int == 0 = "Zero"
--        | otherwise = "Pred " ++ show (negate int) where
--          int = aiToInteger ai


-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where
  (<=) Zero Zero = True
  (<=) (Succ x) (Succ y) = (<=) x y
  (<=) (Pred x) (Pred y) = (<=) x y
  (<=) Zero (Pred _) = False
  (<=) (Pred _) Zero = True
  (<=) Zero (Succ _) = True
  (<=) (Succ _) Zero = False
  (<=) (Succ _) (Pred _) = False
  (<=) (Pred _) (Succ _) = True


-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger (Zero) = 0
aiToInteger (Succ x) = 1 + aiToInteger x
aiToInteger (Pred x) = aiToInteger x - 1


-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero Zero = Zero
plusAbs x Zero = x
plusAbs Zero x = x
plusAbs (Succ x) (Pred y) = plusAbs x y
plusAbs (Pred x) (Succ y) = plusAbs x y
plusAbs x (Pred y) = plusAbs (Pred x) y
plusAbs x (Succ y) = plusAbs (Succ x) y


-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs a@(Succ _) b@(Succ _) = timesAbs' a b
timesAbs a@(Succ _) b@(Pred _) = negate (timesAbs' a (negate b))
timesAbs a@(Pred _) b@(Succ _) = negate (timesAbs' (negate a) b)
timesAbs a@(Pred _) b@(Pred _) = timesAbs' (negate a) (negate b)
timesAbs Zero _ = Zero
timesAbs _ Zero = Zero

timesAbs' :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs' _ Zero = Zero
timesAbs' Zero _ = Zero
timesAbs' x (Succ y) = x + timesAbs' x y
timesAbs' x (Pred y) = timesAbs' x y - x


-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
  (+) = plusAbs
  (*) = timesAbs
  (-) a b = a + negate b
  negate Zero = Zero
  negate (Succ x) = (Pred (negate x))
  negate (Pred x) = (Succ (negate x))
  fromInteger i
      | i == 0 = Zero
      | i < 0 =  negate (fromInteger' (negate i))
      | otherwise = fromInteger' i where
          fromInteger' :: Integer -> AbstractInteger
          fromInteger' 0 = Zero
          fromInteger' x = Succ (fromInteger' (x - 1))
  abs a
      | a < Zero = negate a
      | otherwise = a
  signum s
      | s < Zero = -1
      | s > Zero = 1
      | otherwise = 0


-- Задача 6 -----------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial x = go x 1 where
    go 0 res = res
    go n res = go (n - 1) (res * n)


-- Задача  7 -----------------------------------------
data Quaternion = Quaternion Double Double Double Double deriving (Eq)

instance Show Quaternion where
    show (Quaternion r i j k) = (show r) ++ showIm i ++ "i" ++ showIm j ++ "j" ++ showIm k ++ "k" where
      showIm :: Double -> String
      showIm x
          | x >= 0 = '+': show x
          | otherwise = show x


-- Задача 8 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion r1 i1 j1 k1) (Quaternion r2 i2 j2 k2) = (Quaternion (r1+r2) (i1+i2) (j1+j2) (k1+k2))


-- Задача 9 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion r1 i1 j1 k1) (Quaternion r2 i2 j2 k2) =
  (Quaternion
  (r1*r2 - i1*i2 - j1*j2 - k1*k2) (r1*i2 + i1*r2 + j1*k2 - k1*j2)
  (r1*j2 - i1*k2 + j1*r2 + k1*i2) (r1*k2 + i1*j2 - j1*i2 + k1*r2))


--- Задача 10 ----------------------------------------
instance Num Quaternion  where
    (+) = plusQuaternion
    (*) = timesQuaternion
    negate (Quaternion r i j k) = (Quaternion (negate r) (negate i) (negate j) (negate k))
    fromInteger x = (Quaternion (fromInteger x) 0 0 0)
    abs (Quaternion r i j k) = (Quaternion (sqrt(r*r + i*i + j*j + k*k)) 0 0 0)
    signum (Quaternion r i j k) = (Quaternion (r/sq) (i/sq) (j/sq) (k/sq)) where
        sq = sqrt(r*r + i*i + j*j + k*k)
