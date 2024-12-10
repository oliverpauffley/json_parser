{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Main where

import           Control.Applicative (Alternative (empty, many, (<|>)))
import           Data.Char           (isDigit, isSpace)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- Note: no support for float
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)] -- Alist
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser a) = Parser $ \input -> do
    (input', x) <- a input
    Just (input', f x)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)

  Parser f <*> Parser a = Parser $ \input -> do
    (input', func) <- f input
    (input'', b) <- a input'
    Just (input'', func b)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser a) <|> (Parser b) = Parser (\input -> a input <|> b input)

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true"  = JsonBool True
    f "false" = JsonBool False
    f _       = error "this can't happen"

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where
    f ds = JsonNumber $ read ds

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser
    ( \input ->
        let (token, rest) = span f input
         in Just (rest, token)
    )

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Nothing
    else Just (input', xs)

charP :: Char -> Parser Char
charP x = Parser $ \case
  y : ys | y == x -> Just (ys, x)
  _ -> Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

ws :: Parser String
ws = spanP isSpace

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy sep jsonValue
    sep = ws *> charP ',' <* ws

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
  where
    pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' <* ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO ()
main = putStrLn "Hello, Haskell!"
