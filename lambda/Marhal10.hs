--{-# OPTIONS_GHC -Wall #-}
module Marhal10 where

import Text.ParserCombinators.Parsec
-- Задача 1 -----------------------------------------
evExpr  :: String -> Maybe Integer
evExpr s = case parse (expr <* eof) "" s of
    Left _ -> Nothing
    Right x -> Just x
    where
        opMul :: Parser Char
        opMul = char '*' <|> char '/' <|> char '%'

        opAdd :: Parser Char
        opAdd = char '+' <|> char '-'

        integer :: Parser Integer
        integer = fmap read (many1 digit)

        factor :: Parser Integer
        factor = between (char '(') (char ')') expr <|> integer

        term :: Parser Integer
        term = chainl1 factor (do
            op <- opMul
            case op of
                '*' -> return (*)
                '/' -> return div
                '%' -> return mod)

        expr :: Parser Integer
        expr = do
            op <- option '+' opAdd
            case op of
                '+' -> chainl1 term addTerm
                '-' -> chainl1 (fmap negate term) addTerm
            where
                addTerm = do
                    op <- opAdd
                    case op of
                        '+' -> return (+)
                        '-' -> return (-)

-- Задача 2 -----------------------------------------
data Expr = Add Expr Expr | Sub Expr Expr
          | Mul Expr Expr | Mod Expr Expr | Div Expr Expr
          | Var String | Lit Int
            deriving (Show, Eq)

fullExpr :: Parser Expr
fullExpr = spaces >> exprT <* eof

integer :: Parser Expr
integer = fmap (Lit . read) (many1 digit)

iden :: Parser Expr
iden = fmap Var (do
    l <- letter
    other <- many (digit <|> letter)
    return (l:other))

opMul :: Parser Char
opMul = char '*' <* spaces <|> char '/' <* spaces <|> char '%' <* spaces

opAdd :: Parser Char
opAdd = char '+' <* spaces <|> char '-' <* spaces

factorT :: Parser Expr
factorT = between (char '(') (char ')') (spaces >> exprT) <|> integer <* spaces <|> iden <* spaces

termT :: Parser Expr
termT = chainl1 factorT (do
    op <- opMul
    case op of
        '*' -> return Mul
        '/' -> return Div
        '%' -> return Mod)

exprT :: Parser Expr
exprT = do
    op <- option '+' opAdd
    case op of
        '+' -> chainl1 termT addTermT
        '-' -> chainl1 (fmap (Mul (Lit (-1))) termT) addTermT
        where
            addTermT = do
                op <- opAdd
                case op of
                    '+' -> return Add
                    '-' -> return Sub

astExpr :: String -> Maybe Expr
astExpr str = case parse fullExpr "" str of
               Left _     -> Nothing
               Right expr -> Just expr

-- Задача 3 -----------------------------------------
data RE = Null           | -- Нуль вираз
          Term Char  | -- Термінальний символ
          Seq RE RE | -- Послідовність
          Alt RE RE  | -- Альтернатива
          Rep RE       | -- Повторення (*)
          Plus RE      | -- Повторення (+)
          Opt RE        -- Необов’язкове входження (?)
       deriving (Eq, Show)

reg :: Parser RE
reg = rexpr <* eof

rexpr :: Parser RE
rexpr = chainl1 rterm (char '|' >> return Alt)

rterm :: Parser RE
rterm = chainl1 rfact (return Seq)

rfact :: Parser RE
rfact = do
    p <- prime
    o <- many (char '*' <|> char '+' <|> char '?')
    return $ foldr oFolder p o
    where
        oFolder '*' = Rep
        oFolder '+' = Plus
        oFolder '?' = Opt

prime :: Parser RE
prime = rsymb <|> between (char '(') (char ')') rexpr

rsymb :: Parser RE
rsymb = fmap Term (noneOf "()|*+?")

regExp :: String -> Maybe RE
regExp str = case parse reg "" str of
               Left _   -> Nothing
               Right rg -> Just rg

-- Задача 4 -----------------------------------------
type Name = String
type Attributes = [(String, String)]
data XML  =  Text String | Element Name Attributes [XML]
          deriving (Eq, Show)

wSp :: Parser ()
wSp = spaces

textXML :: Parser XML
textXML = fmap Text (many1 (noneOf "<>"))

valueXML :: Parser String
valueXML = many (noneOf "\"")

nameXML :: Parser Name
nameXML = do
    l <- letter
    other <- many (letter <|> digit <|> char '.' <|> char '-')
    return (l:other)

element :: Parser XML
element = do
    char '<'
    name <- nameXML
    attrs <- many attribute
    char '>'
    xmls <- many xmlXML
    char '<'
    char '/'--anyChar
    nameXML--string name
    char '>'
    return (Element name attrs xmls)

attribute :: Parser (String, String)
attribute = do
    wSp
    name <- nameXML
    wSp
    char '='
    wSp
    char '"'
    value <- valueXML
    char '"'
    return (name, value)


xmlXML :: Parser XML
xmlXML = try element <|> textXML

fullXML  :: Parser XML
fullXML = wSp >> element <* wSp <* eof

anXML :: String -> Maybe XML
anXML str = case parse fullXML "" str of
               Left _    -> Nothing
               Right xml -> Just xml

------------------------------------------------------
re1, re2, re3, re4, re5 :: RE
re1  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2  = Seq (Term 'x') (Rep (Term '\''))
re3  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4  = Seq (Alt (Term 'a') Null) (Term 'a')
re5  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"
{-
<film title="Casablanca">
  <director>Michael Curtiz</director>
  <year>1942</year>
</film>
-}
casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]
