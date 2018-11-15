import Data.Array
type Graph = Array Int [Int]

g2 :: Graph
g2 = listArray(0,4)[[1,2,3],[0,2,3],[0,1,3,4],[0,1,2],[0,2]]

edgeIn :: Graph -> (Int,Int) -> Bool
edgeIn g(x,y) = elem y (g!x)

--cусіди вершини v в графі g
adjacent :: Graph -> Int -> [Int]
adjacent g v = g ! v

-- всі вершини графа g
nodes :: Graph -> [Int]
nodes g = indices g

--всі ребра графу
edges :: Graph-> [(Int,Int)]
edges g = [(v1,v2) | v1<-nodes g, v2 <- g!v1]

subseqs :: [Int] -> [[Int]]
subseqs [] = [[]]
subseqs (x:xs) = [x:xss | xss <- subseqs xs] ++ subseqs xs

allEdges :: [Int] -> [(Int, Int)]
allEdges xs = [(x,y) | x <- xs, y <- xs, x /= y]

isClique :: Graph -> [Int] -> Bool
isClique gr xs = let es = allEdges xs
               in null $ filter (not . (edgeIn gr)) es

cliqueNum :: Graph -> Int
cliqueNum gr = let xss = subseqs (indices gr)
                   qs = filter (isClique gr) xss
               in maximum $ map length qs
