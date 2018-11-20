{-# OPTIONS_GHC -Wall #-}
module Marhal09 where

import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
type Visited = State
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE
simplify (Null) = Null
simplify (Term c) = Term c
simplify (Seq reL reR) = (Seq (simplify reL) (simplify reR))
simplify (Alt reL reR) = (Alt (simplify reL) (simplify reR))
simplify (Rep re) = (Rep (simplify re))
simplify (Plus re) = (Seq (simplify re) (Rep (simplify re)))
simplify (Opt re) = (Alt (simplify re) Null)

-- Задача 2 -----------------------------------------
startState     :: Automation -> State
terminalStates :: Automation -> [State]
transitions    :: Automation -> [Transition]

startState (start, _, _) = start
terminalStates (_, finStates, _) = finStates
transitions (_, _, trans) = trans

-- Задача 3 -----------------------------------------
isTerminal :: State -> Automation -> Bool
isTerminal state (_, finStates, _) = elem state finStates

-- Задача 4 -----------------------------------------
transitionsFrom :: State -> Automation -> [Transition]
transitionsFrom state (_, _, trans) = filter (\(s,_,_) -> (==) state s) trans

-- Задача 5 -----------------------------------------
doLabels :: [Transition] -> [Label]
doLabels [] = []
doLabels ((_,_,label):ts) = [l | l <- label : doLabels ts, l /= Eps]

labels :: [Transition] -> [Label]
labels [] = []
labels trans = let withoutEps = trans
               in doLabels withoutEps

-- Задача 6 -----------------------------------------
stStep   :: Automation -> State -> Label -> [State]
setStep  :: Automation -> [State] -> Label -> [State]
closure  :: Automation -> [State] -> [State]

stStep (_, _, trans) state input = getDestStates $ filter (\(f, _, l) -> f == state && input==l) trans where
  getDestStates :: [Transition] -> [State]
  getDestStates [] = []
  getDestStates ((_, to, _):ts) = nub $ to : (getDestStates ts)
setStep aut states input = concatMap (\state -> stStep aut state input) states
closure aut states = nub $ closure' aut states [] where
  closure' :: Automation -> [State] -> [Visited] -> [State]
  closure' _ [] _ = []
  closure' a sts visited = let transEps = setStep a sts Eps
                           in  transEps ++ (closure' a [t | t <- transEps, notElem t visited] (transEps ++ visited))

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts = undefined

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null start finish nxt =        ([(start, finish, Eps)], nxt)
make (Term c) start finish nxt =    ([(start, finish, C c)], nxt)
make (Seq reL reR) start finish nxt = let (trans1, st1) = make reL start nxt (nxt + 2)
                                          (trans2, strans2) = make reR (nxt + 1) finish st1
                                      in  ((nxt, nxt + 1, Eps) : trans1 ++ trans2, strans2)
make (Alt reL reR) start finish nxt = let (trans1, st1) = make reL nxt (nxt + 1) (nxt + 4)
                                          (trans2, strans2) = make reR (nxt + 2) (nxt + 3) (st1)
                                      in  ((start, nxt, Eps) : (nxt + 1, finish, Eps) : (start, nxt + 2, Eps) : (nxt + 3, finish, Eps) : trans1 ++ trans2, strans2)
make (Rep r) start finish nxt = let (trans1, st1) = make r nxt (nxt + 1) (nxt + 2)
                                in  ((start, nxt, Eps) : (start, finish, Eps) : (nxt + 1, finish, Eps) : (nxt + 1, nxt, Eps) : trans1, st1)
make (Plus _) _ _ _ = undefined -- NEVER USED
make (Opt _) _ _ _ = undefined  -- NEVER USED

-- Задача 9 -----------------------------------------
getFrontier :: State -> Automation -> [Transition]
getFrontier = undefined

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions = undefined

makeDA' :: Automation -> [State] -> [MetaState] -> [MetaTransition]
                   -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined

makeDA :: Automation -> Automation
makeDA  = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigure, re1, re2, re3, re4, re5 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Alt (Term 'a') Null) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),(5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),(10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
