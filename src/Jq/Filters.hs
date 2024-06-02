module Jq.Filters where

import Jq.Json (JSON (JString), indent, prettyPrintJSON)

data Filter
  = Identity
  | Index Filter
  | IterateIndex [Filter]
  | IterateSlice Int Int
  | IterateFull
  | OptionalIndex Filter
  | OptionalIterateFull
  | OptionalIterateIndex [Filter]
  | OptionalIterateSlice Int Int
  | RecursiveDescent
  | Pipe Filter Filter
  | Comma Filter Filter
  | IfThenELse Filter Filter Filter
  | TryCatch Filter Filter
  | Array [Filter]
  | Object [([Char], Filter)]
  | Literal JSON
  | Add Filter Filter
  | Subs Filter Filter
  | Div Filter Filter
  | Mul Filter Filter
  | Mod Filter Filter
  | Neq Filter Filter
  | Eq Filter Filter
  | Smaller Filter Filter
  | And Filter Filter
  | Or Filter Filter
  | Not
  | Parens Filter

instance Show Filter where
  show Identity = "."
  show (Index x) = ".[" ++ show x ++ "]"
  show (IterateIndex []) = ".[]"
  show (IterateIndex (x : xs)) = ".[" ++ show x ++ foldr (\el res -> res ++ ", " ++ show el) "" xs ++ "]"
  show (IterateSlice x y) = ".[" ++ show x ++ ":" ++ show y ++ "]"
  show (OptionalIterateSlice x y) = show (IterateSlice x y) ++ "?"
  show (Pipe x y) = show x ++ " | " ++ show y
  show (Comma x y) = show x ++ " , " ++ show y
  show (IfThenELse x y z) = "if " ++ show x ++ " then " ++ show y ++ " else " ++ show z
  show (TryCatch x y) = "try " ++ show x ++ " catch " ++ show y
  show (OptionalIndex str) = ".[\"" ++ show str ++ "\"]"
  show IterateFull = ".[]"
  show OptionalIterateFull = ".[]?"
  show RecursiveDescent = ".."
  show (Literal x) = show x
  show (Array xs) = prettyPrintJq (Array xs) ""
  show (Object xs) = prettyPrintJq (Object xs) ""
  show (OptionalIterateIndex xs) = show (IterateIndex xs) ++ "?"
  show (Neq x y) = show x ++ " != " ++ show y
  show (Eq x y) = show x ++ " == " ++ show y
  show (Smaller x y) = show x ++ " < " ++ show y
  show (And x y) = show x ++ " and " ++ show y
  show (Or x y) = show x ++ " or " ++ show y
  show Not = "not "
  show (Add op1 op2) = show op1 ++ " + " ++ show op2
  show (Subs op1 op2) = show op1 ++ " - " ++ show op2
  show (Div op1 op2) = show op1 ++ " / " ++ show op2
  show (Mul op1 op2) = show op1 ++ " * " ++ show op2
  show (Mod op1 op2) = show op1 ++ " % " ++ show op2
  show (Parens op) = "(" ++ show op ++ ")"

instance Eq Filter where
  Identity == Identity = True
  (Index x) == (Index y) = x == y
  (Pipe xop1 xop2) == (Pipe yop1 yop2) = (xop1 == yop1) && (xop2 == yop2)
  (Comma xop1 xop2) == (Comma yop1 yop2) = (xop1 == yop1) && (xop2 == yop2)
  -- TODO
  _ == _ = False

prettyPrintJq :: Filter -> [Char] -> [Char]
prettyPrintJq (Array (x : xs)) ind = "[\n" ++ ind ++ "  " ++ show x ++ foldl (\res i -> res ++ ",\n  " ++ ind ++ prettyPrintJq (xs !! i) (indent ++ ind)) "" [0 .. length xs - 1] ++ "\n" ++ ind ++ "]"
prettyPrintJq (Object (x : xs)) ind = "{\n" ++ indent ++ ind ++ "\"" ++ fst x ++ "\": " ++ prettyPrintJq (snd x) (ind ++ indent) ++ foldl (\res (key, value) -> res ++ ",\n" ++ indent ++ ind ++ "\"" ++ key ++ "\": " ++ prettyPrintJq value (ind ++ indent)) "" xs ++ "\n" ++ ind ++ "}"
prettyPrintJq (Literal x) ind = prettyPrintJSON x ind
prettyPrintJq x _ = show x

data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC x = Index $ Literal (JString x)

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma
