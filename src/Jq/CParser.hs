module Jq.CParser where

import Control.Monad (join)
import Jq.Filters
import Jq.JParser
import Jq.Json (JSON (JDouble, JString))
import Parsing.Parsing

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseIndexGeneric :: Parser Filter
parseIndexGeneric = do
  _ <- token . char $ '.'
  _ <- char '['
  index <- parseFilter
  _ <- char ']'
  return $ Index index

parseIndex :: Parser Filter
parseIndex = do
  _ <- token . char $ '.'
  index <- pString <|> some (alphanum <|> char '_')
  return $ Index (Literal (JString index))

parseIndexFull :: Parser Filter
parseIndexFull = do
  _ <- token . char $ '.'
  _ <- symbol "["
  _ <- symbol "]"
  return IterateFull

parseIndexIterator :: Parser Filter
parseIndexIterator = do
  _ <- token . char $ '.'
  _ <- symbol "["
  elements <-
    many $
      do
        el <- int
        _ <- symbol ","
        return el
  final <- int
  _ <- symbol "]"
  return $ IterateIndex (map (Literal . JDouble . fromIntegral) elements ++ [Literal $ JDouble (fromIntegral final)])

parseIndexSlice :: Parser Filter
parseIndexSlice =
  do
    _ <- token . char $ '.'
    do
      _ <- symbol "["
      from <- integer
      _ <- symbol ":"
      to <- integer
      _ <- symbol "]"
      return $ IterateSlice from to
    <|> do
      _ <- symbol "["
      from <- integer
      _ <- symbol ":"
      _ <- symbol "]"
      return $ IterateSlice from maxBound
    <|> do
      _ <- symbol "["
      _ <- symbol ":"
      to <- integer
      _ <- symbol "]"
      return $ IterateSlice 0 to

parseOptionalIndex :: Parser Filter
parseOptionalIndex = do
  _ <- token . char $ '.'
  index <- anystring
  _ <- char '?'
  return $ OptionalIndex (Literal (JString index))

parseOptionalIterateFull :: Parser Filter
parseOptionalIterateFull = do
  _ <- token . char $ '.'
  _ <- symbol "["
  _ <- symbol "]"
  _ <- symbol "?"
  return OptionalIterateFull

parseOptionalIndexGeneric :: Parser Filter
parseOptionalIndexGeneric = do
  _ <- token . char $ '.'
  _ <- symbol "["
  index <- parseFilter
  _ <- symbol "]"
  _ <- symbol "?"
  return $ OptionalIndex index

parseOptionalIterateIndex :: Parser Filter
parseOptionalIterateIndex = do
  _ <- token . char $ '.'
  _ <- symbol "["
  elements <-
    many $
      do
        el <- parseFilter
        _ <- symbol ","
        return el
  final <- parseFilter
  _ <- symbol "]"
  _ <- symbol "?"
  return $ OptionalIterateIndex (elements ++ [final])

parseOptionalIndexSlice :: Parser Filter
parseOptionalIndexSlice =
  do
    filt <- parseIndexSlice
    _ <- symbol "?"
    case filt of
      IterateSlice from to -> return (OptionalIterateSlice from to)

parseRecursiveDescent :: Parser Filter
parseRecursiveDescent =
  do
    _ <- symbol ".."
    return RecursiveDescent

parseJqObject :: Parser Filter
parseJqObject = do
  _ <- char '{'
  elements <-
    many $
      do
        name <- pString <|> many (alphanum <|> char ' ' <|> char '_')
        _ <- symbol ":"
        value <- parseFilter
        _ <- symbol ","
        return (name, value)

  name <- pString <|> many (alphanum <|> char ' ' <|> char '_')
  _ <- symbol ":"
  value <- parseFilter

  _ <- char '}'
  return $ Object (elements ++ [(name, value)])

parseJqObjectEmpty :: Parser Filter
parseJqObjectEmpty = do
  _ <- symbol "{"
  _ <- symbol "}"
  return $ Object []

parseJqArray :: Parser Filter
parseJqArray = do
  _ <- char '['
  elements <-
    many $
      do
        el <- parseFilter
        _ <- char ','
        return el
  final <- parseFilter
  _ <- char ']'
  return $ Array (elements ++ [final])

parseJqArrayEmpty :: Parser Filter
parseJqArrayEmpty = do
  _ <- symbol "["
  _ <- symbol "]"
  return $ Array []

parseLiterals :: Parser Filter
parseLiterals = do
  Literal <$> (token parseJNull <|> parseJDouble <|> parseScientific <|> parseJNumber <|> parseJString <|> parseBool)

parseIfThenElse :: Parser Filter
parseIfThenElse = do
  _ <- symbol "if"
  x <- parseFilter
  _ <- symbol "then"
  y <- parseFilter
  _ <- symbol "else"
  z <- parseFilter
  _ <- symbol "end"
  return $ IfThenELse x y z

parseBoolUniExpr :: Parser Filter
parseBoolUniExpr = do
  _ <- symbol "not"
  return Not

parseParens :: Parser Filter
parseParens = do
  _ <- symbol "("
  x <- parseFilter
  _ <- symbol ")"
  return $ Parens x

parseFilterHelper :: Parser Filter
parseFilterHelper = parseOptionalIndexGeneric <|> parseOptionalIndexSlice <|> parseOptionalIterateIndex <|> parseOptionalIterateFull <|> parseOptionalIndex <|> parseIndexIterator <|> parseIndexFull <|> parseIndexGeneric <|> parseIndexSlice <|> parseIndex <|> parseJqObjectEmpty <|> parseJqObject <|> parseJqArrayEmpty <|> parseJqArray <|> parseRecursiveDescent <|> parseIdentity <|> parseLiterals <|> parseIfThenElse <|> parseBoolUniExpr <|> parseParens

parseFilter :: Parser Filter
parseFilter = do
  first <- parseFilterHelper
  join
    ( do
        _ <- symbol ","
        return $ Comma first <$> parseFilter
        <|> do
          _ <- symbol "|"
          return $ Pipe first <$> parseFilter
        <|> do
          _ <- symbol "*"
          return $ Mul first <$> parseFilter
        <|> do
          _ <- symbol "/"
          return $ Div first <$> parseFilter
        <|> do
          _ <- symbol "%"
          return $ Mod first <$> parseFilter
        <|> do
          pure . Pipe first <$> parseFilter
        <|> do
          _ <- symbol "+"
          return $ Add first <$> parseFilter
        <|> do
          _ <- symbol "-"
          return $ Subs first <$> parseFilter
        <|> do
          _ <- symbol "and"
          return $ And first <$> parseFilter
        <|> do
          _ <- symbol "or"
          return $ Or first <$> parseFilter
        <|> do
          _ <- symbol "=="
          return $ Eq first <$> parseFilter
        <|> do
          _ <- symbol "!="
          return $ Neq first <$> parseFilter
        <|> do
          return $ pure first
    )

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
