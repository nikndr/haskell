{-# OPTIONS_GHC -Wall #-}
module Marhal02 where

  -- Problem 1
sumFl :: [Integer] -> Integer
sumFl = foldl (+) 0

  -- Problem 2
productFr :: [Integer] -> Integer
productFr = foldr (*) 1

  -- Problem 3
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (\x y -> x:y) ys xs

  -- Problem 4
insert :: [Int] -> Int -> [Int]
insert xs x = [i | i <- xs, i < x] ++ [x] ++ [i | i <- xs, i >= x]

sortInsert :: [Int] -> [Int]
sortInsert = foldl insert []

  -- Problem 5
findIndices :: (Int -> Bool) -> [Int] -> [Int]
findIndices p xs = map fst $ filter (p . snd) $ zip [0..] xs

  -- Problem 6
allReverse :: [String] -> [String]
allReverse xss = reverse $ map reverse xss

  -- Problem 7
noDigits :: String -> String
noDigits = filter (\el -> not $ elem el ['0'..'9'])

  -- Problem 8
cntGood ::  [Int -> Bool] -> Int -> Int
cntGood [] _ = 0
cntGood (p:ps) v | p v = 1 + cntGood ps v | otherwise = cntGood ps v

  -- Problem 9
nextRow :: [Integer] -> [Integer]
nextRow xs = zipWith (+) (0:xs) (xs ++ [0])

trianglePas :: [[Integer]]
trianglePas = iterate nextRow [1]

  -- Problem 10
factorialsM :: [Integer]
factorialsM = zipWith (*) [1..] (1:factorialsM)
