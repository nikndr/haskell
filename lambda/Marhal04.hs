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
skipSpace = undefined

-- Задача 2 -----------------------------------------
getAttribute :: String -> XML -> String
getAttribute = undefined

-- Задача 3 -----------------------------------------
getChildren :: String -> XML -> [XML]
getChildren = undefined

-- Задача 4 -----------------------------------------
getChild :: String -> XML -> XML
getChild = undefined

-- Задача 5 -----------------------------------------
addChild :: XML -> XML -> XML
-- Передумова: другий аргумент - завжди побудований конструктором Element
addChild  = undefined

-- Задача 6 -----------------------------------------
getValue :: XML -> XML
getValue = undefined

-- Задача 7 -----------------------------------------
addText :: String -> Stack -> Stack
-- Передумова: Є по крайній мірі один елемент Element в стеку
addText = undefined

-- Задача 8 -----------------------------------------
popAndAdd :: Stack -> Stack
-- Передумова: Є по крайній мірі два елемента Elements в стеку
popAndAdd = undefined

-- Початковий елемент стеку
sentinel :: XML
sentinel = Element "" [] []

-- Задача 9 -----------------------------------------
parseAttributes :: String -> (Attributes, String)
-- Передумова: Рядок, що містить XML-атрибути, синтаксично вірний
parseAttributes  = undefined

-- Аналіз імені елемента/атрибута
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c1 : _)
  | isAlpha c1 = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

-- Задача 10 -----------------------------------------
parse :: String -> XML
-- Передумова: рядок, що містить XML-документ, синтаксично вірний
parse s = parse' (skipSpace s) [sentinel]

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
