{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}
module Text.EasyJson.Parser where

import Control.Applicative
import Control.Monad.Identity
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Parsec hiding (many, (<|>))

import Text.EasyJson.AST

type ParserState = ()
type Parser = ParsecT String ParserState Identity

sstring :: String -> Parser String
sstring = lexeme . string

schar :: Char -> Parser Char
schar = lexeme . char

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

pJson :: Parser Json
pJson = choice [Number <$> pNumber, String <$> pString,
                false, true, null, array, object] where
  enclose l r p = between (schar l) (schar r) $ p `sepEndBy` schar ','
  array = Array . V.fromList <$> enclose '[' ']' pJson
  object = Object . H.fromList <$> enclose '{' '}' keyval
  keyval = (,) <$> pString <*> (schar ':' *> pJson)
  false = sstring "false" >> return (Bool False)
  true = sstring "true" >> return (Bool True)
  null = sstring "null" *> return Null

pNumber :: Parser Double
pNumber = lexeme $ fmap read $ do
  first <- many1 digit
  option first $ do
    dot <- char '.'
    rest <- many1 digit
    return $ first <> [dot] <> rest

pString :: Parser Text
pString = lexeme $ do
  start <- char '"' <|> char '\''
  loop start []
  where
    loop stop acc = do
      option (pack $ reverse acc) $ anyChar >>= \case
        c | c == stop -> return $ pack $ reverse acc
        '\\' -> anyChar >>= \case
          'n' -> add '\n'
          'r' -> add '\r'
          't' -> add '\r'
          'b' -> add '\r'
          '\\' -> add '\\'
          '"' -> add '"'
          '\'' -> add '\''
          c -> unexpected $ "Unrecognized escape sequence: \\" <> [c]
        c -> add c
      where add c = loop stop (c : acc)

parseIt :: String -> Either ParseError Json
parseIt = parse (pJson <* eof) ""