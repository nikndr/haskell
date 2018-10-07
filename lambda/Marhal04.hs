{-# OPTIONS_GHC -Wall #-}
module Marhal04 where

import Data.Char

type Name = String
type Attributes = [(Name, String)]
data XML  = Text String | Element Name Attributes [XML]
         deriving (Eq, Show)
type Stack = [XML]

-- Задача 1 -----------------------------------------
skipSpace :: String -> String
skipSpace = dropWhile isSpace

-- Задача 2 -----------------------------------------
getAttribute :: String -> XML -> String
getAttribute _ (Text _) = ""
getAttribute _ (Element _ [] _) = ""
getAttribute attr (Element n (curA:restA) c)
  | attr == fst curA = snd curA
  | otherwise = getAttribute attr (Element n restA c)

-- Задача 3 -----------------------------------------
getChildren :: String -> XML -> [XML]
getChildren _ (Text _) = []
getChildren _ (Element _ _ []) = []
getChildren name (Element _ _ xs) = [x | x <- xs, getName x == name] where
  getName :: XML -> String
  getName (Text _) = ""
  getName (Element a _ _) = a

-- Задача 4 -----------------------------------------
getChild :: String -> XML -> XML
getChild name el
  | length children > 0 = head $ children
  | otherwise = Text "" where
    children = getChildren name el

-- Задача 5 -----------------------------------------
addChild :: XML -> XML -> XML
addChild _ (Text _) = Text ""
addChild x (Element n a xs) = (Element n a (xs ++ [x]))

-- Задача 6 -----------------------------------------
getValue :: XML -> XML
getValue a = Text (getString a) where
  getString :: XML -> String
  getString (Text t) = t
  getString (Element _ _ xs) = foldl (++) "" [getString x | x <- xs]

-- Задача 7 -----------------------------------------
addText :: String -> Stack -> Stack
-- Передумова - є по крайній мірі один елемент Element в стеку
addText s (st:sts) = (addChild (Text s) st):sts

-- Задача 8 -----------------------------------------
popAndAdd :: Stack -> Stack
-- Передумова: Є по крайній мірі два елемента Elements в стеку
popAndAdd (x:y:xs) = (addChild x y):xs

-- Початковий елемент стеку
sentinel :: XML
sentinel = Element "" [] []

-- Задача 9 -----------------------------------------
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

replace :: String -> String
replace ('\"':xs) = "" ++ replace xs
replace (x:xs)       = x : replace xs
replace ""           = ""

parseAttributes :: String -> (Attributes, String)
parseAttributes s = ([(head xxx, replace (last xxx)) | xxx <- splited2], rest)
 where xs = split '>' s
       ss = init xs
       rest = last xs
       splited1 = concat $ filter (/=[""]) [split ' ' x | x <- ss]
       splited2 = [split '=' xl | xl <- splited1]
-- Аналіз імені елемента/атрибута
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c : cs)
  | isAlpha c = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

-- Задача 10 -----------------------------------------
parse :: String -> XML
parse s
  = parse' (skipSpace s) [sentinel]

parse' :: String -> Stack -> XML
parse' = undefined

-----------------------------------------------------------------------
-- Деякі корисні функції перетворення в рядок і виводу
-- Функція перетворення в рядок ('show' function) для XML об'єктів
showXML :: XML -> String
showXML (Text t) = t
showXML (Element n as es)
     = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
       where
          showAtts ast = concatMap showAtt ast
          showAtt (n1, v) = " " ++ n1 ++ "=" ++ "\"" ++ v ++ "\""
-- Функція перетворення в рядок ('show' function) для списку XML об'єктів
showXMLs :: [XML] -> String
showXMLs = concatMap showXML
-- Функція виводу XML об'єкта на екран
printXML :: XML -> IO()
printXML = putStrLn . showXML

-------------------------------------------------------------------------
-- Тестові дані
-- Прості тести XML-об'єктів (без проміжків)
s1, s2, s3 :: String
s1 = "<a>A</a>"
s2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"
-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a"
            []
            [Element "b"
                     []
                     [Element "c"
                              [("att","att1")]
                              [Text "text1"],
                      Element "c"
                              [("att","att2")]
                              [Text "text2"]],
             Element "b"
                     []
                     [Element "c"
                              [("att","att3")]
                              [Text "text3"],
                      Element "d"
                              []
                              [Text "text4"]]]

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- XML-документ з Мал.1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Результат аналізу  попереднього докуменнту ('parse films')
filmsParsed :: XML
filmsParsed
  = Element "filmlist"
            []
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")]
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")]
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]
