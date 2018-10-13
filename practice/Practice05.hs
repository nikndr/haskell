type Expr = [Term]
type Term = [String]

breaks :: [a] -> [[[a]]]
breaks [x] = [[[x]]]
breaks (x:xs) = [[x]:part | part <- breaks xs] ++ [(x:ys):yss | (ys:yss) <- breaks xs]

build :: String -> [Expr]
build = concatMap breaks . breaks

eval :: Expr -> Int
eval = sum . map evalTerm where
  evalTerm :: Term -> Int
  evalTerm = product . map read

showE :: Expr -> String
showE = tail . concatMap (('+':) . showT) where
  showT :: Term -> String
  showT = tail . concatMap ('*':)

findG :: Int -> String -> [String]
findG v = map showE . find v where
  find
