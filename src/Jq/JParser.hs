module Jq.JParser where

import Control.Monad (join)
import Jq.Json
import Parsing.Parsing

parseJNull :: Parser JSON
parseJNull = do
  _ <- string "null"
  return JNull

parseJNumber :: Parser JSON
parseJNumber = do
  number <- integer
  return $ JDouble $ fromIntegral number

parseJDouble :: Parser JSON
parseJDouble = do
  first <- integer
  _ <- char '.'
  second <- nat
  if second == 0 then return $ JDouble (fromIntegral first) else return $ JDouble (fromIntegral first + sign first * fromIntegral second * (10 ** order second)) -- Weird hacking dw about it
  where
    order x = -1 - fromIntegral (floor (logBase 10.0 (fromIntegral x)))
    sign x = if x >= 0 then 1 else -1

parseScientific :: Parser JSON
parseScientific = do
  first <- integer
  _ <- char 'E'
  do
    second <- integer
    return $
      JDouble (fromIntegral first * (10 ** fromIntegral second))
    <|> do
      _ <- symbol "+"
      second <- integer
      return $
        JDouble (fromIntegral first * (10 ** fromIntegral second))
    <|> do
      _ <- symbol "-"
      second <- integer
      return $
        JDouble (fromIntegral first * (10 ** (-fromIntegral second)))

parseJString :: Parser JSON
parseJString = do
  JString <$> pString

parseBool :: Parser JSON
parseBool = do
  parseTrue <|> parseFalse

parseTrue :: Parser JSON
parseTrue = do
  _ <- string "true"
  return $ JBool True

parseFalse :: Parser JSON
parseFalse = do
  _ <- string "false"
  return $ JBool False

parseArray :: Parser JSON
parseArray = do
  parseArrayEmpty <|> parseArrayFull

parseArrayEmpty :: Parser JSON
parseArrayEmpty = do
  _ <- char '['
  _ <- char ']'
  return $ JArray []

parseArrayFull :: Parser JSON
parseArrayFull = do
  _ <- char '['
  elements <-
    many $
      do
        el <- parseJSON
        _ <- char ','
        return el
  final <- parseJSON
  _ <- char ']'
  return $ JArray $ elements ++ [final]

parseObject :: Parser JSON
parseObject = do
  parseObjectEmpty <|> parseObjectFull

parseObjectFull :: Parser JSON
parseObjectFull = do
  _ <- char '{'
  elements <-
    many $
      do
        name <- pString
        _ <- symbol ":"
        value <- parseJSON
        _ <- symbol ","
        return (name, value)

  name <- pString
  _ <- symbol ":"
  value <- parseJSON

  _ <- char '}'
  return $ JObject (elements ++ [(name, value)])

parseObjectEmpty :: Parser JSON
parseObjectEmpty = do
  _ <- char '{'
  _ <- char '}'
  return $ JObject []

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseJDouble <|> parseScientific <|> parseJNumber <|> parseJString <|> parseBool <|> parseArray <|> parseObject
