module JParserTest (jParserTests) where

import Jq.JParser (parseJSON)
import Jq.Json (JSON (..))
import Parsing.Parsing (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Prelude hiding (fail)

jParserTests :: TestTree
jParserTests =
  testGroup
    "JParser tests"
    [ testCase "nullTest" $ "null" `parseTo` JNull,
      testCase "integerTest" $ "3" `parseTo` JInt 3,
      testCase "doubleTest" $ "3.14" `parseTo` JDouble 3.14,
      testCase "scientificTest" $ "3E-2" `parseTo` JDouble 0.03,
      testCase "stringTest" $ "\"hello\"" `parseTo` JString "hello",
      testCase "singleCharString" $ "\"h\"" `parseTo` JString "h",
      testCase "failure" $ fail "tnull",
      testCase "boolTrue" $ "true" `parseTo` JBool True,
      testCase "boolFalse" $ "false" `parseTo` JBool False,
      testCase "ArrayEmpty" $ "[]" `parseTo` JArray [],
      testCase "ArrayOne" $ "[3]" `parseTo` JArray [JInt 3],
      testCase "ArrayMul" $ "[3, 3, 3]" `parseTo` JArray [JInt 3, JInt 3, JInt 3],
      testCase "ObjectEmpty" $ "{}" `parseTo` JObject [],
      testCase "ObjectOne" $ "{\"hello\": 1}" `parseTo` JObject [("hello", JInt 1)],
      testCase "ObjectMul" $ "{\"hello\": 1, \"hi\": 2}" `parseTo` JObject [("hello", JInt 1), ("hi", JInt 2)]
    ]

parseTo :: String -> JSON -> Assertion
parseTo s j = case parse parseJSON s of
  [(v, "")] -> assertEqual ("Expected:\n" ++ show j ++ "\ngot:\n" ++ show v) j v
  [(v, s)] -> assertFailure $ "Parsed:\n" ++ show v ++ "\nwith remainder:\n" ++ show s
  _ -> assertFailure "Parsing failed"

fail :: String -> Assertion
fail s = case parse parseJSON s of
  [(v, "")] -> assertFailure $ "Parsing should fail but succeeded with:\n" ++ show v
  _ -> return ()
