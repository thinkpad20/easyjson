{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}
module Text.EasyJson.Combinators where

import Data.Monoid
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Text.EasyJson.AST
import Text.EasyJson.Parser

type JError = Text
type JReader = Either JError

showT :: Show a => a -> Text
showT = T.pack . show

json :: Text -> JReader Json
json s = case parseIt (T.unpack s) of
  Left perr -> Left $ "Parse error: " <> showT perr
  Right j -> return j

key :: Text -> Json -> JReader Json
key k j = case j of
  Object o -> case H.lookup k o of
    Just j -> return j
    Nothing -> Left $ "No key '" <> k <> "'"
  _ -> Left $ "Expected an object, but got " <> jsonType j

index :: Int -> Json -> JReader Json
index i j = case j of
  Array a
    | i < V.length a -> return $ a V.! i
    | otherwise -> Left $ "Index " <> showT i <> " out of range"
  _ -> Left $ "Expected an array, but got " <> jsonType j

letter :: Int -> Json -> JReader Char
letter i j = case j of
  String s
    | i < T.length s -> return $ T.index s i
    | otherwise -> Left $ "Index " <> showT i <> " out of range"
  _ -> Left $ "Expected a string, but got " <> jsonType j

-- For extracting
array :: Json -> JReader (Vector Json)
array j = case j of
  Array a -> return a
  _ -> Left $ "Expected an array, but got " <> jsonType j

object :: Json -> JReader (HashMap Text Json)
object j = case j of
  Object o -> return o
  _ -> Left $ "Expected an object, but got " <> jsonType j

number :: Json -> JReader Double
number j = case j of
  Number n -> return n
  _ -> Left $ "Expected a number, but got " <> jsonType j

string :: Json -> JReader Text
string j = case j of
  String s -> return s
  _ -> Left $ "Expected a string, but got " <> jsonType j

bool :: Json -> JReader Bool
bool j = case j of
  Bool b -> return b
  _ -> Left $ "Expected a bool, but got " <> jsonType j

null :: Json -> JReader ()
null j = case j of
  Null -> return ()
  _ -> Left $ "Expected a null, but got " <> jsonType j
