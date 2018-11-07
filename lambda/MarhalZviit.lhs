> import Data.List

Перша задача, яку ми розглянемо – обрахунок запису у зворотній польській нотації.
Щоб обчислити вираз 10 - (4 + 3) * 2, нам треба спочатку привести його до вигляду, який буде "зрозумілий комп'ютеру": 10 4 3 + 2 * -
Тепер, використавши стек, ми можемо легко порахувати значення: кладемо операнди і в стек; зойно натрапляємо на оператор, проводимо відповідне обчислення і результат кладемо в стек.

Щоб обчислити подібний вираз в хаскелі, потрібно скласти відповідну функцію

> solveRPN' :: (Num a, Read a) => String -> a
> solveRPN' = head . foldl foldingFunction [] . words
>     where   foldingFunction (x:y:ys) "*" = (x * y):ys
>             foldingFunction (x:y:ys) "+" = (x + y):ys
>             foldingFunction (x:y:ys) "-" = (y - x):ys
>             foldingFunction xs numberString = read numberString:xs

Тут ми одразу розбиваємо рядок на оператори і операнди функцією words і за допомогою функції foldl згортаємо стек, поки не отримаємо результат
Функція має досить простий вигляд і працює лише для трьох операторів. Якщо нам на вхід приходить те, чого немає в переліку операторів, ми його записуємо назад в стек.

Результати виконання функції:
ghci> solveRPN "10 4 3 + 2 * -"
-4
ghci> solveRPN "2 3 +"
5
ghci> solveRPN "90 34 12 33 55 66 + * - +"
-3947
ghci> solveRPN "90 34 12 33 55 66 + * - + -"
4037
ghci> solveRPN "90 34 12 33 55 66 + * - + -"
4037
ghci> solveRPN "90 3 -"
87

Трохи покращимо функцію, додавши операторів, в тому числі різної арності:

> solveRPN :: String -> Float
> solveRPN = head . foldl foldingFunction [] . words
>     where   foldingFunction (x:y:ys) "*" = (x * y):ys
>             foldingFunction (x:y:ys) "+" = (x + y):ys
>             foldingFunction (x:y:ys) "-" = (y - x):ys
>             foldingFunction (x:y:ys) "/" = (y / x):ys
>             foldingFunction (x:y:ys) "^" = (y ** x):ys
>             foldingFunction (x:xs) "ln" = log x:xs
>             foldingFunction xs "sum" = [sum xs]
>             foldingFunction xs numberString = read numberString:xs

Наступна задача має назву "З Гітроу до Лондона" і пов'язана з пошуком найкоротшого шляху між двома точками.
Ми маємо дві головні дороги (А і Б) від аеропорта Гітроу до Лондона, які перетинають регіональні дороги.
Наше завдання – знайти найкоротший шлях між цими двома точками, використовуючи як головні так і другорядні дороги.

Введемо позначення для побудови нашого маршруту: A - рухаємося по дорозі A; B - рухаємося по дорозі B; C - повертаємо на сусідню дорогу.

Визначимо новий тип Path:

> data Label = A | B | C deriving (Show)
> type Path = [(Label, Int)]

Також побудуємо нашу систему доріг, визначивши відповідні структуру і тип:

> data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
> type RoadSystem = [Section]

Тоді дорога, як на рисунку, матиме вигляд:

> heathrowToLondon :: RoadSystem
> heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

Ми будемо розв'язувати цю задачу таким чином: з початкової точки знайдемо найкоротший шлях до А1 і В1.
Маючи ці знання, так само знайдемо шлях до А2 і В2. В кінці-кінців, коли ми дійдемо до An i Bn, ми матимемо оптимальний шлях

Визначимо функцію, яка приймає попередню ітерацію шляху і повертає наступну оптимальну:

> roadStep :: (Path, Path) -> Section -> (Path, Path)
> roadStep (pathA, pathB) (Section a b c) =
>     let priceA = sum $ map snd pathA
>         priceB = sum $ map snd pathB
>         forwardPriceToA = priceA + a
>         crossPriceToA = priceB + b + c
>         forwardPriceToB = priceB + b
>         crossPriceToB = priceA + a + c
>         newPathToA = if forwardPriceToA <= crossPriceToA
>                         then (A,a):pathA
>                         else (C,c):(B,b):pathB
>         newPathToB = if forwardPriceToB <= crossPriceToB
>                         then (B,b):pathB
>                         else (C,c):(A,a):pathA
>     in  (newPathToA, newPathToB)

Тепер визначимо функцію, що обраховує оптимальний шлях від початку до кінця шляху

> optimalPath :: RoadSystem -> Path
> optimalPath roadSystem =
>     let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
>     in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
>             then reverse bestAPath
>             else reverse bestBPath

Щоб правильно обробляти введені дані, нам потрібна ще одна функція, яка розіб'є список на групи по n складових.
В нашій задачі користуватимемося розбиттям на групи по 3.

> groupsOf :: Int -> [a] -> [[a]]
> groupsOf 0 _ = undefined
> groupsOf _ [] = []
> groupsOf n xs = take n xs : groupsOf n (drop n xs)

Головний модуль матиме вигляд:

> main = do
>     contents <- getContents
>     let threes = groupsOf 3 (map read $ lines contents)
>         roadSystem = map (\[a,b,c] -> Section a b c) threes
>         path = optimalPath roadSystem
>         pathString = concat $ map (show . fst) path
>         pathPrice = sum $ map snd path
>     putStrLn $ "The best path to take is: " ++ pathString
>     putStrLn $ "The price is: " ++ show pathPrice

50  10  30  5  90  20  40  2  25  10  8  0  
