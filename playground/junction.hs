sort :: Ord a => [a] -> [a]
sort (x:xs) = sort [y | y <- xs, y < x ] ++
                   [x                  ] ++
              sort [y | y <- xs, y >= x]

fibs 0 = 0
fibs 1 = 1
fibs n = fibs (n - 1) + fibs (n - 2)
fibonacci n = take n [x | x <- [1..]]


primes = sieve (2:[3,5..]) where sieve (p:ps) = p : sieve [x | x <- ps, mod x p > 0]
primesLessThan x = takeWhile (<x) primes
nthPrime x = last $ take x primes
fn24 :: Double -> Bool
fn24 x = let sq = sqrt x
         in (sq - floor (sqrt x)) == 0

sqr = floor . sqrt

true = (\a b -> a)
false = (\a b -> b)
if' = (\f a b -> f a b)
and' = (\a b -> a `false` b)
or' = (\a b -> a `true` b)
