{-# OPTIONS_GHC -Wall #-}
module Marhal08 where

import Data.Array
import Data.List

type Graph = Array Int [Int]
type Vertice = Int
type Path = [Vertice]

isVisited :: Vertice -> Path -> Bool
isVisited vertice path = elem vertice path

getPaths :: [[Path]] -> [Path]
getPaths pathss | null pathss = [] | otherwise = foldr1 (++) pathss

allPaths :: Graph -> Vertice -> Vertice -> Path -> [Path]
allPaths gr a b visited = if isVisited a visited then
                          []
                      else
                          let vAdj = adjacent gr a
                              paths = map (\v -> [a:path | path <- allPaths gr v b (a:visited)]) vAdj
                              curPaths = if edgeIn gr (a,b) then [[a,b]] else []
                          in curPaths ++ getPaths paths


adjacent :: Graph -> Int -> [Int]
adjacent g v = g ! v

nodes :: Graph -> [Int]
nodes g = indices g

allEdges :: [Int] -> [(Int, Int)]
allEdges xs = [(x,y) | x <- xs, y <- xs, x /= y]

edges :: Graph -> [(Int,Int)]
edges g = [(v1,v2) | v1 <- nodes g, v2 <- g!v1]

edgeIn :: Graph -> (Int,Int) -> Bool
edgeIn g (x,y) = elem y (g!x)

verPairs :: Graph -> [(Int, Int)]
verPairs gr = [(a,b) | a <- nodes gr, b <- nodes gr]

-- Задача 1 ------------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay graph a b = let paths = allPaths graph a b []
                        sorted = sortBy (compareLength) paths
                    in if length sorted > 0 then Just $ head sorted else Nothing where
                        compareLength path1 path2
                            | length path1 < length path2 = GT
                            | length path1 > length path2 = LT
                            | otherwise = EQ


-- Задача 2 -----------------------------------------
isNoCycle :: Graph -> Bool
isNoCycle gr = filter (\vert -> null $ allPaths gr vert vert []) (nodes gr) == nodes gr

-- Задача 3 -----------------------------------------
isTransitive :: Graph -> Bool
isTransitive gr = filter (\(a, b) -> (null $ allPaths gr a b []) || (edgeIn gr (a,b))) (verPairs gr) == verPairs gr

-- Задача 4 -----------------------------------------
isGraph :: Graph -> Bool
isGraph gr = foldl1 (&&) [edgeIn gr (b, a) | (a, b) <- edges gr]

-- Задача 5 -----------------------------------------
shortWay :: Graph -> Int -> Int -> Maybe [Int]
shortWay graph a b = let paths = allPaths graph a b []
                         sorted = sortBy (compareLength) paths
                     in if length sorted > 0 then Just $ head sorted else Nothing where
                        compareLength path1 path2
                            | length path1 > length path2 = GT
                            | length path1 < length path2 = LT
                            | otherwise = EQ

-- Задача 6 -----------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr = isGraph gr && foldl1 (&&) [not . null $ allPaths gr a b [] | (a, b) <- verPairs gr]

-- Задача 7 -----------------------------------------
components :: Graph -> [[Int]]
components = undefined

-- Задача 8 -----------------------------------------
topolSorting :: Graph -> Maybe [Int]
topolSorting = undefined


gr1, gr2, gr3, gr4, gr5 :: Graph
gr1 = array (1,9) [(1,[2]), (2,[3,5]), (3, [7]), (4, [7]), (5, [8]), (6, [9]), (7, [8]), (8, [3]), (9, [])]

gr2 = array (1,9) [(1,[2]), (2,[3,5]), (3, [7]), (4, [7]), (5, [8]), (6, [9]), (7, []), (8, [3]), (9, [])]

gr3 = array (1,4) [(1, [2,3,4]), (2, [3,4]), (3, [4]), (4, [])]
gr4 = array (1,4) [(1, [2,3,4]), (2, [3]), (3, [4]), (4, [])]

gr5 = array (1,9) [(1,[2]), (2,[1,3,5]), (3, [2,7,8]), (4, [7]), (5, [2,8]), (6, [9]), (7, [3,4,8]), (8, [3,5,7]), (9, [6])]
