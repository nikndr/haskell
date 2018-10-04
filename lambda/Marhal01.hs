{-# OPTIONS_GHC -Wall #-}
module HWP01 where

-- Problem 1
power3 :: [Integer]
power3 = [x^3 | x <- [1..]]

-- Problem 2
toPower3 :: [Integer]
toPower3 = [3^x | x <- [1..]]

-- Problem 3
sumPower3 :: Integer -> Integer
sumPower3 n = sum [3^x | x <- [1..n]]

--- Problem 4
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum [m^x | x <- [1..n]]

-- Problem 5
lessMe :: [Int] -> [Int]
lessMe [] = []
lessMe xs = map (\el -> (length . filter (< el)) xs) xs

-- Problem 6
frequency :: [Int] -> [(Int,Int)]
frequency [] = []
frequency (x:xs) = (x, length . filter (== x) $ x:xs) : frequency (filter (/= x) $ xs)

-- Problem 7
hailstone :: Int -> Int
hailstone n | even n = n `div` 2 | otherwise = 3 * n + 1

-- Problem 8
hailSeq :: Int -> [Int]
hailSeq x | x == 1 = [1] | otherwise = x: hailSeq (hailstone x)

-- Problem 9
allHailSeq :: [[Int]]
allHailSeq = [xs | xs <- hailSeq `map` [1..]]

-- Problem 10
firstHailSeq :: Int -> Int
firstHailSeq l = (head . head) (filter (\x -> length x == l) allHailSeq)
