module Jq.Compiler where

import Data.Ratio
import Jq.Filters
import Jq.Json

type JProgram a = JSON -> Either String a

compileIndex :: Filter -> JProgram [JSON]
compileIndex (Index _) (JArray []) = Left "Index out of range"
compileIndex (Index (Literal (JDouble 0))) (JArray (x : _)) = return [x]
compileIndex (Index (Literal (JDouble index))) (JArray (_ : xs)) = compile (Index (Literal (JDouble (index - 1)))) (JArray xs)
compileIndex (Index _) (JObject []) = return [JNull]
compileIndex (Index (Literal (JString index))) (JObject (x : _)) | fst x == index = return [snd x]
compileIndex (Index (Literal (JString index))) (JObject (_ : xs)) = compile (Index (Literal (JString index))) (JObject xs)
compileIndex (Index (Index x)) inp = compileRecursiveIndex (Index (Index x)) inp
compileIndex (Index (Pipe op1 op2)) inp = compileRecursiveIndex (Index (Pipe op1 op2)) inp
compileIndex (Index (Comma op1 op2)) inp = compileRecursiveIndex (Index (Pipe op1 op2)) inp
compileIndex (Index _) JNull = return [JNull]
compileIndex (Index index) x = Left ("Failed to index: " ++ show x ++ "\n" ++ show index)
compileIndex op x = Left ("Something went horribly wrong: " ++ show x ++ "\n" ++ show op)

compileRecursiveIndex :: Filter -> JProgram [JSON]
compileRecursiveIndex (Index x) inp = case compile x inp of
  Left err -> Left err
  Right [] -> return [JNull]
  Right [res] -> compile (Index (Literal res)) inp
  Right xs -> compileIterateIndex (IterateIndex (map Literal xs)) inp
compileRecursiveIndex _ x = Left ("Something went horribly wrong: " ++ show x)

compileIndexFull :: Filter -> JProgram [JSON]
compileIndexFull IterateFull (JArray inp) = compileIterateIndex (IterateIndex (map (Literal . JDouble . fromIntegral) [0 .. (length inp - 1)])) (JArray inp)
compileIndexFull IterateFull (JObject []) = return []
compileIndexFull IterateFull (JObject ((_, value) : xs)) = case compileIndexFull IterateFull (JObject xs) of
  Left x -> Left x
  Right x -> return (value : x)
compileIndexFull op x = Left ("Something went horribly wrong: " ++ show x ++ "\n" ++ show op)

compileComma :: Filter -> JProgram [JSON]
compileComma (Comma op1 op2) inp =
  case (compile op1 inp, compile op2 inp) of
    (Right x, Right y) -> return (x ++ y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
compileComma _ inp = Left ("Something went horribly wrong: " ++ show inp)

compilePipe :: Filter -> JProgram [JSON]
compilePipe (Pipe op1 op2) inp =
  case compile op1 inp of
    Left y -> Left y
    Right xs -> mapPipe op2 xs []
compilePipe op inp = Left ("Something went horribly wrong: " ++ show inp ++ "\n" ++ show op)

mapPipe :: Filter -> [JSON] -> [JSON] -> Either String [JSON]
mapPipe _ [] res = return res
mapPipe op (x : xs) res =
  case compile op x of
    Left compiled -> Left compiled
    Right compiled -> mapPipe op xs (res ++ compiled)

maxi :: [Filter] -> Int -> Int
maxi [] curr_max = curr_max
maxi ((Literal (JDouble y)) : xs) curr_max
  | x > curr_max = maxi xs x
  | otherwise = maxi xs curr_max
  where
    x = round y
maxi _ _ = -1

compileIterateIndex :: Filter -> JProgram [JSON]
compileIterateIndex (IterateIndex xs) (JArray ys) =
  return
    [ if round i < 0
        then ys !! (length ys + round i)
        else ys !! round i
      | (Literal (JDouble i)) <- xs,
        round i < length ys
    ]
compileIterateIndex _ inp = Left ("Something went horribly wrong: " ++ show inp)

compileIndexSlice :: Filter -> JProgram [JSON]
compileIndexSlice (IterateSlice x y) (JArray xs) | from >= 0 = return [JArray (take (to - from) (drop from xs))]
  where
    from = if x >= 0 then x else length xs + x
    to = if y >= 0 then y else length xs + y
compileIndexSlice (IterateSlice x y) (JString xs) | from >= 0 = return [JString (take (to - from) (drop from xs))]
  where
    from = if x >= 0 then x else length xs - x
    to = if y >= 0 then y else length xs - y
compileIndexSlice _ inp = Left ("Something went horribly wrong: " ++ show inp)

compileOptional :: Filter -> JProgram [JSON]
compileOptional (OptionalIndex (Literal (JString str))) (JObject xs) = compileIndex (Index (Literal (JString str))) (JObject xs)
compileOptional _ JNull = return [JNull]
compileOptional _ _ = return []

compileOptionalIterateFull :: Filter -> JProgram [JSON]
compileOptionalIterateFull OptionalIterateFull (JArray xs) = compileIndexFull IterateFull (JArray xs)
compileOptionalIterateFull OptionalIterateFull (JObject xs) = compileIndexFull IterateFull (JObject xs)
compileOptionalIterateFull _ _ = return []

compileOptionalIterateSlice :: Filter -> JProgram [JSON]
compileOptionalIterateSlice (OptionalIterateSlice x y) (JArray xs) = compileIndexSlice (IterateSlice x y) (JArray xs)
compileOptionalIterateSlice (OptionalIterateSlice x y) (JString xs) = compileIndexSlice (IterateSlice x y) (JString xs)
compileOptionalIterateSlice _ JNull = return [JNull]
compileOptionalIterateSlice _ _ = return []

compileArray :: Filter -> JProgram [JSON]
compileArray (Array (x : _)) inp = case compile x inp of
  Left res -> Left res
  Right res -> return [JArray res]
compileArray (Array []) _ = return [JArray []]
compileArray _ inp = Left ("Something went horribly wrong: " ++ show inp)

compileObject :: Filter -> JProgram [JSON]
compileObject (Object ((key, x) : _)) inp = case compile x inp of
  Left res -> Left res
  Right res -> return [JObject [(key, head res)]]
compileObject (Object []) _ = return [JObject []]
compileObject _ inp = Left ("Something went horribly wrong " ++ show inp)

compileOptionalIterateIndex :: Filter -> JProgram [JSON]
compileOptionalIterateIndex (OptionalIterateIndex xs) (JArray ys) = return [if round i < 0 then ys !! (length ys + round i) else ys !! round i | (Literal (JDouble i)) <- xs, round i < length ys]
compileOptionalIterateIndex (OptionalIterateIndex xs) (JString ys) = case compileIterateIndex (IterateIndex xs) (JString ys) of
  Left _ -> return [JNull]
  Right x -> return x
compileOptionalIterateIndex (OptionalIterateIndex _) JNull = return [JNull]
compileOptionalIterateIndex (OptionalIterateIndex _) _ = return []
compileOptionalIterateIndex _ inp = Left ("Something went horribly wrong " ++ show inp)

compileRecursiveDescent :: Filter -> JProgram [JSON]
compileRecursiveDescent RecursiveDescent (JArray xs) = case arr of
  Left x -> Left x
  Right ys -> return (JArray xs : concat ys)
  where
    arr = traverse (compileRecursiveDescent RecursiveDescent) xs
compileRecursiveDescent RecursiveDescent (JObject xs) = case arr of
  Left x -> Left x
  Right ys -> return (JObject xs : concat ys)
  where
    arr = traverse (compileRecursiveDescent RecursiveDescent . snd) xs
compileRecursiveDescent RecursiveDescent x = return [x]
compileRecursiveDescent _ inp = Left ("Something went horribly wrong " ++ show inp)

compileIfThenElse :: Filter -> JProgram [JSON]
compileIfThenElse (IfThenELse x y z) inp = case compile x inp of
  Left res -> Left res
  Right xs -> ifThenElse xs y z inp []
compileIfThenElse _ inp = Left ("Something went horribly wrong " ++ show inp)

ifThenElse :: [JSON] -> Filter -> Filter -> JSON -> [JSON] -> Either String [JSON]
ifThenElse [] _ _ _ ys = Right ys
ifThenElse (x : xs) y z inp ys | x == JNull || x == JBool False = case compile z inp of
  Left res -> Left res
  Right res -> ifThenElse xs y z inp (res ++ ys)
ifThenElse (_ : xs) y z inp ys = case compile y inp of
  Left res -> Left res
  Right res -> ifThenElse xs y z inp (res ++ ys)

compileIntExpr :: Filter -> Filter -> Filter -> JProgram [JSON]
compileIntExpr op op1 op2 inp = case compile op1 inp of
  Left x -> Left x
  Right left -> case compile op2 inp of
    Left x -> Left x
    Right right -> case op of
      (Add _ _) -> addArithmExpr left right []
      (Subs _ _) -> subtArithmExpr left right []
      (Mul _ _) -> multArithmExpr left right []
      (Div _ _) -> divArithmExpr left right []
      (Mod _ _) -> modArithmExpr left right []
      _ -> Left (show op ++ " Not yet implemented")

addArithmExpr :: [JSON] -> [JSON] -> [JSON] -> Either String [JSON]
addArithmExpr [] _ res = return res
addArithmExpr _ [] res = return res
addArithmExpr (x : xs) ys res = addArithmExpr xs ys (map (add x) ys ++ res)

add :: JSON -> JSON -> JSON
add (JDouble x) (JDouble y) = JDouble (x + y)
add JNull y = y
add x JNull = x
add (JArray xs) (JArray ys) = JArray $ xs ++ ys
add (JObject xs) (JObject ys) = JObject $ xs ++ ys

subtArithmExpr :: [JSON] -> [JSON] -> [JSON] -> Either String [JSON]
subtArithmExpr [] _ res = return res
subtArithmExpr _ [] res = return res
subtArithmExpr (x : xs) ys res = subtArithmExpr xs ys (map (subt x) ys ++ res)

subt :: JSON -> JSON -> JSON
subt (JDouble x) (JDouble y) = JDouble (x - y)
subt JNull y = y
subt x JNull = x

multArithmExpr :: [JSON] -> [JSON] -> [JSON] -> Either String [JSON]
multArithmExpr [] _ res = return res
multArithmExpr _ [] res = return res
multArithmExpr (x : xs) ys res = subtArithmExpr xs ys (map (mult x) ys ++ res)

mult :: JSON -> JSON -> JSON
mult (JDouble x) (JDouble y) = JDouble (x * y)
mult JNull y = y
mult x JNull = x

divArithmExpr :: [JSON] -> [JSON] -> [JSON] -> Either String [JSON]
divArithmExpr [] _ res = return res
divArithmExpr _ [] res = return res
divArithmExpr (x : xs) ys res = subtArithmExpr xs ys (map (divi x) ys ++ res)

divi :: JSON -> JSON -> JSON
divi (JDouble x) (JDouble y) = JDouble (x / y)
divi JNull y = y
divi x JNull = x

modArithmExpr :: [JSON] -> [JSON] -> [JSON] -> Either String [JSON]
modArithmExpr [] _ res = return res
modArithmExpr _ [] res = return res
modArithmExpr (x : xs) ys res = subtArithmExpr xs ys (map (subt x) ys ++ res)

modu :: JSON -> JSON -> JSON
modu (JDouble x) (JDouble y) = JDouble $ fromInteger (mod (round x) (round y))
modu JNull y = y
modu x JNull = x

compileBoolExpr :: Filter -> JProgram [JSON]
compileBoolExpr (Eq x y) inp = case compile x inp of
  Left res1 -> Left res1
  Right res1 -> case compile y inp of
    Left res2 -> Left res2
    Right res2 -> return [JBool (res1 == res2)]
compileBoolExpr Not inp = return (notExpr [inp])
compileBoolExpr (And x y) inp = case compile x inp of
  Left res -> Left res
  Right xs -> andExpr xs y inp []
compileBoolExpr (Or x y) inp = case compile x inp of
  Left res -> Left res
  Right xs -> orExpr xs y inp []
compileBoolExpr (Smaller x y) inp = case compile x inp of
  Left res1 -> Left res1
  Right res1 -> case compile y inp of
    Left res2 -> Left res2
    Right res2 -> return [JBool (sort res1 res2)]
compileBoolExpr _ inp = Left ("Something went horribly wrong " ++ show inp)

andExpr :: [JSON] -> Filter -> JSON -> [JSON] -> Either String [JSON]
andExpr [] _ _ res = return res
andExpr (x : xs) op inp res | x == JNull || x == JBool False = andExpr xs op inp (JBool False : res)
andExpr (_ : xs) op inp res = case compile op inp of
  Left ys -> Left ys
  Right ys -> andExpr xs op inp (res ++ ys)

orExpr :: [JSON] -> Filter -> JSON -> [JSON] -> Either String [JSON]
orExpr [] _ _ res = return res
orExpr (x : xs) op inp res | x == JNull || x == JBool False = case compile op inp of
  Left ys -> Left ys
  Right ys -> orExpr xs op inp (res ++ ys)
orExpr (_ : xs) op inp res = orExpr xs op inp (JBool True : res)

notExpr :: [JSON] -> [JSON]
notExpr [] = []
notExpr (x : xs) | x == JNull || x == JBool False = JBool True : notExpr xs
notExpr (_ : xs) = JBool False : notExpr xs

sort :: [JSON] -> [JSON] -> Bool
sort xs ys = undefined

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile (Index x) inp = compileIndex (Index x) inp
compile (Comma op1 op2) inp = compileComma (Comma op1 op2) inp
compile (Pipe op1 op2) inp = compilePipe (Pipe op1 op2) inp
compile (IterateIndex xs) inp = compileIterateIndex (IterateIndex xs) inp
compile (IterateSlice from to) inp = compileIndexSlice (IterateSlice from to) inp
compile (OptionalIterateSlice from to) inp = compileOptionalIterateSlice (OptionalIterateSlice from to) inp
compile (OptionalIndex str) inp = compileOptional (OptionalIndex str) inp
compile IterateFull inp = compileIndexFull IterateFull inp
compile OptionalIterateFull inp = compileOptionalIterateFull OptionalIterateFull inp
compile (OptionalIterateIndex x) inp = compileOptionalIterateIndex (OptionalIterateIndex x) inp
compile RecursiveDescent inp = compileRecursiveDescent RecursiveDescent inp
compile (IfThenELse x y z) inp = compileIfThenElse (IfThenELse x y z) inp
compile (Array xs) inp = compileArray (Array xs) inp
compile (Object xs) inp = compileObject (Object xs) inp
compile (Literal x) _ = return [x]
compile (Add op1 op2) inp = compileIntExpr (Add op1 op2) op1 op2 inp
compile (Subs op1 op2) inp = compileIntExpr (Subs op1 op2) op1 op2 inp
compile (Mul op1 op2) inp = compileIntExpr (Mul op1 op2) op1 op2 inp
compile (Div op1 op2) inp = compileIntExpr (Div op1 op2) op1 op2 inp
compile (Mod op1 op2) inp = compileIntExpr (Mod op1 op2) op1 op2 inp
compile (Neq x y) inp = compile (Pipe (Eq x y) Not) inp
compile (Eq x y) inp = compileBoolExpr (Eq x y) inp
compile (And x y) inp = compileBoolExpr (And x y) inp
compile (Or x y) inp = compileBoolExpr (Or x y) inp
compile Not inp = compileBoolExpr Not inp
compile (Parens op) inp = compile op inp
compile _ _ = Left "Not yet implemented"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
