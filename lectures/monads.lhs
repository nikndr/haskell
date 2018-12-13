Вступ до монад
1. Архів даних

> data Person = []
> mother, father :: Person -> Maybe Person
> mf, mff :: Person -> Maybe Person -- mf - дідусь по лінії матері, mff - прадід по лінії матері
> mf p = case mother p of
>     Nothing -> Nothing
>     Just m -> father m
> mff p = case mother p of
>     Nothing -> Nothing
>     Just m -> case father m of
>           Nothing -> Nothing
>           Just mf -> father mf

=== === === === === === === === === === === === === === === === === === === ===

> comb :: Maybe a -> (a -> Maybe a) -> Maybe a
> comb Nothing = Nothing
> comb (Just x) f = f x
> mf p = ((Just p) `comb` mother) `comb` father

> mff p = mf `comb` father

=== === === === === === === === === === === === === === === === === === === ===

2. Клас Monad

> class Applicative m => Monad m where
>   return :: a -> m a
>   (>>=) :: m a -> (a -> m b) -> m b
>   (>>) :: m a -> m b -> m b
>   fail :: String -> m a

=== === === === === === === === === === === === === === === === === === === ===

> instance Monad Maybe where
>   return x = Just x
>   Nothing >>= _f = Nothing
>   Just x >>= f = f x

=== === === === === === === === === === === === === === === === === === === ===

> mf p = return p >>= mother >>= father
> mff p = mother p >>= mother >>= father
> mf p = do m <- mother p
>           father m
> mff p = do m <- mother p
>           mf <- father m
>           father mf
> mff p = mother p >>= (\m -> father m >>= (\mf -> father mf))

=== === === === === === === === === === === === === === === === === === === ===

> f1 :: a -> m a
> x <- f1 a -- under do
> f2 :: m a
> f2 -- under do
> f3 :: a -> b
> let b = f3 a -- under do
> g1, g2 :: a -> m a
> p :: Bool
> b <- if p then g1 a -- under
>           else g2 a -- do

=== === === === === === === === === === === === === === === === === === === ===

> mff p = do { m <- mother p; mf <- father m; father mf }
> class Applicative f => Alternative f where
>   empty :: f a
>   (<|>) :: f a -> f a -> f a
>   some, many :: f a -> f [a]

=== === === === === === === === === === === === === === === === === === === ===

> instance Alternative Maybe where
>   empty = Nothing
>   Nothing (<|>) r = r
>   e <|> _ = e -- отака кухня

=== === === === === === === === === === === === === === === === === === === ===

3. Клас MonadPlus

> class (Alternative m, Monad m) => MonadPlus m where
>   mempty :: m a
>   mzero :: Nothing
>   mplus :: m a -> m a -> m a

=== === === === === === === === === === === === === === === === === === === ===

> instance MonadPlus Maybe where
>   mempty = Nothing
>   Nothing `mempty` r = r
>   e `mempty` _ = e -- отака кухня

=== === === === === === === === === === === === === === === === === === === ===

> parent :: Person -> Maybe Person
> parent p = mother p <|> father p -- <|> == `mplus`
> first :: [Maybe a] -> Maybe a
> first = foldl mplus mzero

=== === === === === === === === === === === === === === === === === === === ===

> data Either a b = Left a | Right b
> instance Monad (Either a) where
>   return x = Right x
>   Left x >>= f = Left x
>   Right x >>= f = f x
