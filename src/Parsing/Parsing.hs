{-# OPTIONS_GHC -w #-}

-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing.Parsing (module Parsing.Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

-- Sequencing parsers

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

-- Making choices

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many (alphanum <|> char '_')
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (-n)
    <|> nat

-- Handling spacing

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

anystring :: Parser String
anystring = many (alphanum <|> char '_' <|> char ' ')

uni :: Int -> [Char] -> Int
uni _ [] = 0
uni pow (x : xs) = (digitToInt x * 16 ^ pow) + uni (pow + 1) xs

parseUnicode :: Parser String
parseUnicode =
  do
    string "\\u"
    a <- alphanum
    b <- alphanum
    c <- alphanum
    d <- alphanum
    str <- parseString
    return (toEnum (uni 0 [d, c, b, a]) : str)

anystr :: Parser String
anystr =
  do
    char <- item <|> char ' '
    str <- parseString
    return (char : str)

parseString :: Parser String
parseString =
  do
    parseUnicode <|> escapeString <|> closeString <|> anystr

escapeString :: Parser String
escapeString =
  do
    _ <- char '\\'
    escaped <- char '\"' <|> char 'r' <|> char 'n' <|> char 't' <|> char 'b' <|> char 'f'
    str <- parseString
    return (['\\', escaped] ++ str)

pString :: Parser String
pString =
  do
    _ <- openString
    parseString

openString :: Parser ()
openString = do
  _ <- char '"'
  return ()

closeString :: Parser String
closeString = do
  _ <- char '"'
  return []