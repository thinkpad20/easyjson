{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}
module Text.EasyJson.AST where

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.List (intercalate)
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V

data Json = Number Double
          | String Text
          | Bool Bool
          | Null
          | Array (Vector Json)
          | Object (HashMap Text Json)
          deriving (Eq)

instance Show Json where
  show json = case json of
    Number n -> show n
    String s -> show s
    Bool True -> "true"
    Bool False -> "false"
    Null -> "null"
    Array js -> surround "[" "]" $ V.toList $ fmap show js
    Object obj -> surround "{" "}" $ map showT $ H.toList obj
    where surround l r list = l <> intercalate "," list <> r
          showT (n, j) = show n <> ":" <> show j

jsonType :: Json -> Text
jsonType = \case
  Number _ -> "number"
  String _ -> "string"
  Bool _ -> "bool"
  Null -> "null"
  Array _ -> "array"
  Object _ -> "object"