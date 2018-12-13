{-Functor-}
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap


-- Pryklad odyn
(*2) <$> [3, 7, 10] = [6, 14, 20]

{-Applicative-}

class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f
  (*>) :: f a -> f b -> f b

instance Applicative Maybe where
  pure x = Just x
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  (Just f) <*> (Just x) = Just (f x)

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]


-- Pryklad
[(*2), (+2)] <*> [7, 8, 10] = [14, 16, 20, 9, 10, 12]
((+) <$> Just 2) <*> Nothing -> Nothing

extend :: [String] -> [String]
extend xs = (:) <$> "abc" <*> xs
