{-# OPTIONS_GHC -Wall #-}
module Marhal06 where

import Data.Maybe
import qualified Data.Map as M

-- Всі програми і їх елементи являються вірними (well-formed) в наступному значенні:
--   Всі оператори, функції і процедури застосовуються
--      до вірної кількості аргументів, кожний з яких має відповідний тип.
--   Вирази, які повинні обчислювати логічні значення, будуть завжди обчислюватися
--     до 0 (false) або 1 (true).
--   В присвоєнні масиву  a[i] = e масив завжди визначений (в області дії).
--   Процедура, що викликається як x := p(e1, …, en), завжди буде повертати значення
--     (закінчує своє обчислення оператором return e)
--   Оператор return завжди останній оператор для виконання в блоку процедури
--     (тобто немає "мертвого коду")

--------------------------------------------------------------------
type Id = String
data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value |
           Var Id |
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp]
         deriving (Eq, Show)

data VarDef = Arr Id | Int Id   deriving (Eq, Show)
type FunDef = (Id, ([VarDef], Exp))

type Binding = M.Map Id Value
type StateP = ([Binding],Binding)
-- st = ([locn,.. loc1], glob)  стек локальних записів активацій + глобальний запис активації

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

-- Задача 1 -----------------------------------------
getValue :: Id -> StateP -> Value
-- Передумова: Значення змінної Id є в стані StateP
getValue name state = (M.!) (M.union (getLocals state) (getGlobals state)) (name)

-- Задача 2 -----------------------------------------
getLocals :: StateP -> Binding
getLocals st = foldl M.union (M.fromList []) $ fst st

getGlobals :: StateP -> Binding
getGlobals = snd

-- Задача 3 -----------------------------------------
assignArray :: Value -> Value -> Value -> Value
-- Аргументи - масив, індекс і (нове) значення відповідно
-- Передумова: Три аргумента (Value)  мають значення відповідного типу
--     (масив (A),  ціле (I) і ціле (I)) відповідно.
assignArray (I _) _ _ = undefined
assignArray (A _) (I _) (A _) = undefined
assignArray (A _) (A _) _ = undefined
assignArray (A arr) (I i) (I val) = A (if res == arr then ((i, val):res) else res) where
  assign :: (Int, Int) -> (Int, Int)
  assign el = if fst el == i then (fst el, val) else el
  res = map assign arr

-- Задача 4 -----------------------------------------
updateVar :: (Id, Value) -> StateP -> StateP
updateVar (name, value) st = if M.lookup name (getGlobals st) == Nothing
                                then if null (fst st)
                                        then ([M.fromList [(name, value)]], getGlobals st)
                                        else ((M.insert name value (getLocals st)) : (tail (fst st)), getGlobals st)
                                else (fst st, M.insert name value (getGlobals st))

-- Задача 5 -----------------------------------------
applyOp :: Op -> Value -> Value -> Value
-- Передумова: Значення мають відповідні типи (I або A) для кожної операції
applyOp Add (I x) (I y) = I (x + y)
applyOp Minus (I x) (I y) = I (x - y)
applyOp Mul (I x) (I y) = I (x * y)
applyOp Less (I x) (I y) = if x < y then I 1 else I 0
applyOp Equal (I x) (I y) = if x == y then I 1 else I 0
applyOp Index (A a) (I i) = I (fromMaybe 0 (lookup i a))
applyOp _ _ _ = undefined
-- Задача 6 -----------------------------------------
bindArgs :: [Id] -> [Value] -> Binding
-- Передумова: списки мають однакову довжину
bindArgs idx = M.fromList . zip idx

-- Задача 7 -----------------------------------------
eval :: Exp -> [FunDef] -> StateP -> Value
eval (Const c) _ _ = c
eval (Var name) _ st  = getValue name st
eval (Cond cond true false) dfx st = if eval cond dfx st == I 0 then eval false dfx st else eval true dfx st
eval (OpApp op arg1 arg2) dfx st = applyOp op (eval arg1 dfx st) (eval arg2 dfx st)
eval (FunApp f es) dfx st = eval ef dfx (bindArgs (map initv as) vs : fst st, getGlobals st) where
  (as, ef) = lookUp f dfx
  vs = evalArgs es dfx st
  initv (Arr name) = name
  initv (Int name) = name


evalArgs :: [Exp] -> [FunDef] -> StateP -> [Value]
evalArgs es dfx st = map evalE es where
  evalE e = eval e dfx st

-- Задача 8 -----------------------------------------
executeStatement :: Statement -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeStatement (Assign name ev) dfx _ st = updateVar (name, eval ev dfx st) st
executeStatement (AssignA name ei ev) dfx _ st = updateVar (name, newArr) st where
  newArr = assignArray (getValue name st) (eval ei dfx st) (eval ev dfx st)
executeStatement (If cond btrue bfalse) dfx dpx st = if eval cond dfx st == I 0
                                                          then executeBlock bfalse dfx dpx st
                                                          else executeBlock btrue dfx dpx st
executeStatement (While p b) dfx dpx st             = if eval p dfx st == I 0
                                                        then st
                                                        else executeStatement (While p b) dfx dpx (executeBlock b dfx dpx st)
executeStatement (Call resId pid es) dfx dpx st = if null resId
                                                     then (fst st, snd stProc)
                                                     else updateVar (resId, getValue "$res" stProc) (fst st, snd stProc) where
  (as, bl) = lookUp pid dpx
  vs = evalArgs es dfx st
  initv (Arr idv) = idv
  initv (Int idv) = idv
  stProc = executeBlock bl dfx dpx (bindArgs (map initv as) vs : fst st, getGlobals st)
executeStatement (Return e) dfx _ st = updateVar ("$res", eval e dfx st) st

executeBlock :: Block -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeBlock bx dfx dpx st = foldl foldBlock st bx where
  foldBlock st' statement = executeStatement statement dfx dpx st'

---------------------------------------------------------------------
-- Допоміжні функції і дані для тестування...
-- Функція пошуку...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nНе знайдено  " ++ show x ))
              (lookup x t)

-- Стан для тестування
sampleState :: StateP
sampleState = ([M.fromList [("x",I 5)]], M.fromList [("y",I 2),("a", listToVal [4,2,7])])

-- Перетворює список цілих в масив Value...
listToVal :: [Int] -> Value
listToVal xs = A (zip [0..] xs)

 -- Перетворює ціле в Exp...
intToExp :: Int -> Exp
intToExp n = Const (I n)

-- Реалізація виконання програми
program :: Program -> StateP
program (dvx, dfx, dpx) =
   let initv :: VarDef -> (Id, Value)
       initv (Arr v) = (v, A [])
       initv (Int v) = (v, I 0)
       gl = M.fromList (map initv dvx)
   in executeStatement (Call "" "main" []) dfx dpx ([],gl)

-- fib чиста функція
-- Функція fib, що обчислює число Фібоначчі
-- func  fib(n) =
--     (n < 3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
     ([Int "n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )

-- Масив
sampleArray :: Exp
sampleArray = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1 = ("sumA1",
     ([Arr "a", Int "n"],
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s")
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd = ("gAdd",
     ([Int "x", Int "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Повна програма
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],[Call "" "gAdd" [intToExp 5, intToExp 10] ]))])
