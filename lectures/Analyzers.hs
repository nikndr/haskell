{-1.Parser-}

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

item :: Parser Char
item = Parser (\s -> case s of
  "" -> Nothing
  (c:cs) -> Just (c, cs))

sat :: (Char -> Bool) -> Parser Char
sat p = Parser(\s -> case s of
  "" -> Nothing
  (c:cs) -> if p c then Just (c, cs) else Nothing)

parse item "abc" = Just('a', "bc")
parse item "" = Nothing

char :: Char -> Parser Char
char c = sat(==c)

instance Monad Parser where
  return a = Parser(\s -> Just (a,s))
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser (\s -> case parse p s of
    Nothing -> Nothing
    Just (a, s) -> let Parser g = f a
                    in g s)

pab :: Parser Char
pab = do
  _ <- char 'a'
  x <- item
  _ <- char 'b'
  return x

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\s -> case parse p of
  Nothing -> parse q s
  res -> res)

sign :: Parser String
sign = do { _ <- char '-'; return "-" }
            <|> reuturn ""

many, some :: Parser a -> Parser [a]
some p = do a <- p
            as <- many p
            return (a:as)

many = some p <|> return []

parse (many digit) "123ab" = Just ("123", "ab")
parse (many digit) "ab" = Just ([], "ab")
parse (some digit) "123ab" = Just ("123", "ab")
parse (some digit) "ab" = Nothing

number :: Parser Int
number = do c <- sign
            ds <- some digit
            return (read (c++ds))

parse :: Parser a -> String -> String -> Either ParserError
space, digit, letter :: Parser Char
