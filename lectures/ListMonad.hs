instance Monad [] where
  return a = [a]
  xs >>= f = concat $ map f xs

a12 :: Int -> [Int]
a12 x = [x+1, x+2]

all12 :: [Int]
all12 = do x <- [10, 20, 30]
           a12

all12 = [10, 20, 30] >>= a12 =
  concatMap a12 [10, 20, 30] =
  concat [a12 10, a12 20, a12 30] =
  concat [[11, 12], [21, 22], [32, 32]] =
  [11, 12, 21, 22, 31, 32]

all12' = [y | x <- [10, 20, 30], y <- a12 x]

--all12 and all12' have same result

{-
[exp | q1 .. 1n]
qi  pat <- exp1
    exp2
    let y = exp3
-}

pr :: [Int] -- prosti 4usla
pr = [p | p <- [2..], all (\x -> mod p x /= 0) [2..p-1]]

-- guard

class Monad m => MonadPlus m where
  zero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  zero = []
  mplus = (++)

instance MonadPlus Maybe where
  zero = Nothing
  mplus Nothing x = x
  mplus x _ = x

{-
  [()]
    [], [()], [(),()] …
  Maybe ()
    Nothing
    Just ()
-}

guard :: MonadPlus m => Bool -> m ()
guard True = return ()
guard False = mzero

guard (5>2) :: Maybe () = Just ()
guard (5>7) :: Maybe () = Nothing

guard (5>2) :: [()] = [()]

guard (5>7) :: [()] = []


do { guard t; return 1 } :: [Int]
guard t >>= (\_ -> return 1) :: [Int]
concat (map (\_ -> [1]) guard t) :: [Int]
{-t = True -}
concat (map (\_ -> [1]) [()])
{-t = False -}
concat (map (\_ -> [1]) [])

seven :: [Int]
seven = do x <- [1..50]
           guard ('7' `elem` show x)
           return x

seven = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
