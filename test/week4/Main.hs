{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Data.Either (fromRight, isLeft, isRight)
import Data.List
import GHC.Generics (Generic, Generic1)
import Jq.Compiler
  ( compile,
    run,
  )
import Jq.Filters
  ( Filter (..),
    filterCommaSC,
    filterIdentitySC,
    filterPipeSC,
    filterStringIndexingSC,
  )
import Jq.Json
  ( JSON (..),
    jsonArraySC,
    jsonBoolSC,
    jsonNullSC,
    jsonNumberSC,
    jsonObjectSC,
    jsonStringSC,
  )
import System.Exit
import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

{--
  It can be that one (or both) of these two derivation fail.
  Especially if you introduce some non-trivial constructors
  or if your definition of filter is mutually recursive with
  some other definition.
  This doesn't necessarily mean that you're doing anything wrong.
  You can try fixing it yourself by adding
  `deriving instance Generic X` and
  `deriving instance NFData X` below for the missing classes.
  In case this doesn't work reach out to the course team.
--}
deriving instance Generic JSON

deriving instance NFData JSON

instance Arbitrary JSON where
  arbitrary = do
    n <- arbitrary :: Gen Int
    s <- arbitrary :: Gen String
    b <- arbitrary :: Gen Bool
    xs <- frequency [(1, return []), (5, do x <- arbitrary; return [x])]
    ys <- frequency [(1, return []), (5, do x <- arbitrary; s <- arbitrary; return [(s, x)])]

    elements [jsonNullSC, jsonNumberSC n, jsonStringSC s, jsonBoolSC b, jsonArraySC xs, jsonObjectSC ys]

deriving instance Generic Filter

deriving instance NFData Filter

instance Arbitrary Filter where
  arbitrary = do
    id <- arbitrary :: Gen String
    f <- arbitrary
    frequency [(5, return filterIdentitySC), (5, return (filterStringIndexingSC id)), (1, return (filterPipeSC f f)), (1, return (filterCommaSC f f))]

main = defaultMain tests

tests =
  testGroup
    "Week 4 tests"
    [ testGroup
        "Constructors are defined"
        [ testProperty "Constructor for identity computes" prop_computes_identity,
          testProperty "Constructor for indexing computes" prop_computes_indexing,
          testProperty "Constructor for pipe computes" prop_computes_pipe,
          testProperty "Constructor for comma computes" prop_computes_comma
        ],
      testGroup
        "Reflection instances"
        [ testProperty "Reflection identity" prop_identity_refl,
          testProperty "Reflection indexing" prop_indexing_refl,
          testProperty "Reflection pipe" prop_pipe_refl,
          testProperty "Reflection comma" prop_comma_refl
        ],
      testGroup
        "Identity"
        [ testProperty "Identity functionality" prop_identity
        ],
      testGroup
        "Indexing"
        [ testProperty "Indexing existing keys" prop_index_existent,
          testProperty "Indexing non-existing keys" prop_index_non_existent,
          testProperty "Indexing null" prop_index_null,
          testProperty "Indexing numbers" prop_index_number,
          testProperty "Indexing strings" prop_index_string,
          testProperty "Indexing booleans" prop_index_bool,
          testProperty "Indexing arrays" prop_index_array
        ],
      testGroup
        "Pipe"
        [ testProperty "Pipe with identity on the right" prop_pipe_identity_right,
          testProperty "Pipe with identity on the left" prop_pipe_identity_left,
          testProperty "An error on the left of the pipe leads to an error" prop_pipe_carries_error
        ],
      testGroup
        "Comma"
        [ testProperty "Comma with identical filters leads to duplicate output" prop_comma_duplicates,
          testProperty "An error on the left of the comma leads to an error" prop_comma_carries_error,
          testProperty "Comma with two identities duplcates the input" prop_comma_identity
        ]
    ]

prop_computes_identity = total $ filterIdentitySC

prop_computes_indexing id = total $ filterStringIndexingSC id

prop_computes_pipe f g = total $ filterPipeSC f g

prop_computes_comma f g = total $ filterCommaSC f g

prop_identity_refl = filterIdentitySC == filterIdentitySC

prop_indexing_refl f g =
  f
    == g
    ==> filterStringIndexingSC f
    == filterStringIndexingSC g

prop_pipe_refl e f g h =
  e
    == g
    && f
    == h
    ==> filterPipeSC e f
    == filterPipeSC g h

prop_comma_refl e f g h =
  e
    == g
    && f
    == h
    ==> filterCommaSC e f
    == filterCommaSC g h

prop_identity j = run (compile filterIdentitySC) j == Right [j]

prop_index_existent s j = run (compile $ filterStringIndexingSC s) (jsonObjectSC [(s, j)]) == Right [j]

prop_index_non_existent s t j =
  s
    /= t
    ==> run (compile $ filterStringIndexingSC t) (jsonObjectSC [(s, j)])
    == Right [jsonNullSC]

prop_index_null s = run (compile $ filterStringIndexingSC s) jsonNullSC == Right [jsonNullSC]

prop_index_number n s = isLeft $ run (compile $ filterStringIndexingSC s) (jsonNumberSC n)

prop_index_string s t = isLeft $ run (compile $ filterStringIndexingSC s) (jsonStringSC t)

prop_index_bool b s = isLeft $ run (compile $ filterStringIndexingSC s) (jsonBoolSC b)

prop_index_array j s = isLeft $ run (compile $ filterStringIndexingSC s) (jsonArraySC [j])

prop_pipe_identity_right f j = run (compile $ filterPipeSC f filterIdentitySC) j == compile f j

prop_pipe_identity_left f j = run (compile $ filterPipeSC filterIdentitySC f) j == compile f j

prop_pipe_carries_error f g j =
  isLeft (run (compile f) j)
    ==> isLeft (run (compile $ filterPipeSC f g) j)

prop_comma_duplicates f j =
  let res = run (compile f) j
   in isRight res
        ==> run (compile $ filterCommaSC f f) j
        == Right (fromRight [] res ++ fromRight [] res)

prop_comma_carries_error f g j =
  isLeft (run (compile f) j)
    ==> isLeft (run (compile $ filterCommaSC f g) j)

prop_comma_identity j = run (compile $ filterCommaSC filterIdentitySC filterIdentitySC) j == Right [j, j]
