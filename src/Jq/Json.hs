module Jq.Json where

data JSON
  = JNull
  | JDouble Double
  | JString [Char]
  | JBool Bool
  | JArray [JSON]
  | JObject [([Char], JSON)]

instance Show JSON where
  show JNull = "null"
  show (JDouble x) = if x == fromInteger (round x) then show (round x) else show x
  show (JString x) = "\"" ++ x ++ "\""
  show (JBool True) = "true"
  show (JBool False) = "false"
  show (JArray []) = "[]"
  show (JArray (x : xs)) = prettyPrintJSON (JArray (x : xs)) ""
  show (JObject []) = "{}"
  show (JObject (x : xs)) = prettyPrintJSON (JObject (x : xs)) ""

instance Eq JSON where
  JNull == JNull = True
  (JDouble x) == (JDouble y) = x == y
  (JString x) == (JString y) = x == y
  (JBool x) == (JBool y) = x == y
  (JArray []) == (JArray []) = True
  (JArray []) == (JArray _) = False
  (JArray _) == (JArray []) = False
  (JArray (x : xs)) == (JArray (y : ys)) = x == y && JArray xs == JArray ys
  (JObject []) == (JObject []) = True
  (JObject []) == (JObject _) = False
  (JObject _) == (JObject []) = False
  (JObject (x : xs)) == (JObject (y : ys)) = fst x == fst y && snd x == snd y && xs == ys
  _ == _ = False

prettyPrintJSON :: JSON -> String -> [Char]
prettyPrintJSON (JArray (x : xs)) ind = "[\n" ++ ind ++ "  " ++ prettyPrintJSON x (ind ++ indent) ++ foldl (\res i -> res ++ ",\n  " ++ ind ++ prettyPrintJSON (xs !! i) (indent ++ ind)) "" [0 .. length xs - 1] ++ "\n" ++ ind ++ "]"
prettyPrintJSON (JObject (x : xs)) ind = "{\n" ++ indent ++ ind ++ "\"" ++ fst x ++ "\": " ++ prettyPrintJSON (snd x) (ind ++ indent) ++ foldl (\res (key, value) -> res ++ ",\n" ++ indent ++ ind ++ "\"" ++ key ++ "\": " ++ prettyPrintJSON value (ind ++ indent)) "" xs ++ "\n" ++ ind ++ "}"
prettyPrintJSON x _ = show x

indent :: [Char]
indent = "  "

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to JSON datatype
-- For the tests to succeed fill them in with functions that return correct JSON values
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC = JDouble . fromIntegral

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject
