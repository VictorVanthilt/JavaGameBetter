module CParserTest (cParserTests) where

import Jq.CParser (parseFilter)
import Jq.Filters (Filter (..))
import Jq.Json (JSON (JInt, JString))
import Parsing.Parsing (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Prelude hiding (fail)

cParserTests :: TestTree
cParserTests =
  testGroup
    "CParser tests"
    [ testCase "identityTest" $ "." `parseTo` Identity,
      testCase "indexGenericNumber" $ ".[3]" `parseTo` Index (Literal $ JInt 3),
      testCase "commaTest" $ ".[\"foo_bar\"] , ." `parseTo` Comma (Index $ Literal $ JString "foo_bar") Identity,
      testCase "failure" $ fail ""
    ]

parseTo :: String -> Filter -> Assertion
parseTo s j = case parse parseFilter s of
  [(v, "")] -> assertEqual ("Expected:\n" ++ show j ++ "\ngot:\n" ++ show v) j v
  [(v, s)] -> assertFailure $ "Parsed:\n" ++ show v ++ "\nwith remainder:\n" ++ show s
  _ -> assertFailure "Parsing failed"

fail :: String -> Assertion
fail s = case parse parseFilter s of
  [(v, "")] -> assertFailure $ "Parsing should fail but succeeded with:\n" ++ show v
  _ -> return ()
