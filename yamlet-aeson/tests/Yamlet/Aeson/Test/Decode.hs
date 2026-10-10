module Yamlet.Aeson.Test.Decode
  ( decodeTests
  ) where

import Control.Applicative
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Yamlet

import Yamlet.Aeson
import Yamlet.Aeson.Test.Helpers

decodeTests :: TestTree
decodeTests =
  testGroup
    "decode"
    [ testCase "scalar keys" test_scalarKeys
    , testCase "keys with the same text" test_sameText
    , testCase "collection keys" test_collectionKeys
    , testCase "special floats" test_specialFloats
    , testCase "tags" test_tags
    , testCase "aliases" test_aliases
    , testCase "merge key" test_mergeKey
    , testCase "error location" test_errorLocation
    , testCase "error of a key of a map" test_keyOfMap
    , testCase "error with a message that ignores the value" test_constantMessage
    , testCase "error at a missing key" test_missingKey
    , testCase "error path beyond the node" test_pathBeyondNode
    , testCase "field of a derived record" test_derivedField
    , testCase "types of aeson" test_aesonTypes
    ]

test_scalarKeys :: Assertion
test_scalarKeys =
  assertEqual
    "the text of each key"
    ( Right $
        A.object
          ["0x10" A..= 'a', "true" A..= 'b', "~" A..= 'c', "" A..= 'd', "1.0" A..= 'e']
    )
    (decodeText @A.Value "0x10: a\ntrue: b\n~: c\n'': d\n'1.0': e\n")

test_sameText :: Assertion
test_sameText = do
  assertEqual
    "a number and a string"
    [(2, 1, "duplicate key \"1\" after conversion"), (1, 1, "the first key 1")]
    (errorsOf (decodeText @A.Value "1: a\n\"1\": b\n"))
  assertEqual
    "a string with a tag"
    [(2, 4, "duplicate key \"a\" after conversion"), (1, 1, "the first key \"a\"")]
    (errorsOf (decodeText @A.Value "a: 1\n!x a: 2\n"))

test_collectionKeys :: Assertion
test_collectionKeys =
  assertEqual
    "an error at each key"
    [ (1, 3, "expected a scalar key, but got a list")
    , (3, 3, "expected a scalar key, but got a mapping")
    ]
    (errorsOf (decodeText @A.Value "? [1]\n: a\n? {b: c}\n: d\n"))

test_specialFloats :: Assertion
test_specialFloats = do
  assertEqual
    "the values"
    (Right (A.toJSON [A.String "+inf", A.String "-inf", A.Null, A.Number 0]))
    (decodeText @A.Value "[.inf, -.inf, .nan, -0.0]")
  assertEqual
    "the doubles"
    (Right ["Infinity", "-Infinity", "NaN", "0.0"])
    ((\(ViaAeson ds) -> map (show @Double) ds) <$> decodeText "[.inf, -.inf, .nan, -0.0]")

test_tags :: Assertion
test_tags =
  assertEqual
    "the values without tags"
    ( Right $
        A.object
          ["x" A..= ("abc" :: T.Text), "y" A..= ("1" :: T.Text), "z" A..= (2 :: Int)]
    )
    (decodeText @A.Value "!point {x: !secret abc, y: !!str 1, z: !!int 2}")

test_aliases :: Assertion
test_aliases =
  assertEqual
    "the alias gives the value of its anchor"
    (Right (A.object ["a" A..= [1 :: Int], "b" A..= [1 :: Int]]))
    (decodeText @A.Value "a: &x [1]\nb: *x\n")

test_mergeKey :: Assertion
test_mergeKey =
  assertEqual
    "an ordinary key"
    (Right (A.object ["<<" A..= A.object ["a" A..= (1 :: Int)], "b" A..= (2 :: Int)]))
    (decodeText @A.Value "<<: {a: 1}\nb: 2\n")

test_errorLocation :: Assertion
test_errorLocation = do
  let result =
        decodeText @(ViaAeson [Server]) "- port: 80\n  host: a\n- port: http\n  host: b\n"
  assertEqual
    "the error at the value"
    [(3, 9, "parsing Int failed, expected Number, but encountered String")]
    (errorsOf result)
  assertEqual
    "the path of the value"
    ["[1].port"]
    (pathsOf result)
  where
    pathsOf :: Either (NE.NonEmpty Error) a -> [String]
    pathsOf = \case
      Left errs -> [renderPath err.path | err <- NE.toList errs]
      Right _ -> []

test_keyOfMap :: Assertion
test_keyOfMap = do
  assertEqual
    "an error of the key at the value"
    [(2, 6, "parsing Int failed, Unexpected 'a' while parsing number literal")]
    (errorsOf (decodeText @(ViaAeson (M.Map Int T.Text)) "1: a\nabc: x\n"))
  assertEqual
    "an error of the value at the value"
    [(2, 4, "parsing Int failed, expected Number, but encountered String")]
    (errorsOf (decodeText @(ViaAeson (M.Map Int Int)) "1: 2\n3: x\n"))

test_constantMessage :: Assertion
test_constantMessage =
  assertEqual
    "the error at the value"
    [(1, 7, "invalid port")]
    (errorsOf (decodeText @(ViaAeson Endpoint) "port: http\n"))

test_missingKey :: Assertion
test_missingKey =
  assertEqual
    "the error at the mapping"
    [ (3, 3, "parsing Yamlet.Aeson.Test.Helpers.Server(Server) failed, key \"host\" not found")
    ]
    (errorsOf (decodeText @(ViaAeson [Server]) "- port: 80\n  host: a\n- port: 81\n"))

test_pathBeyondNode :: Assertion
test_pathBeyondNode =
  assertEqual
    "the error at the deepest node with the rest of the path"
    [(1, 3, "parsing Int failed, expected Number, but encountered String at outer[0].inner")]
    (errorsOf (decodeText @(ViaAeson [Nested]) "- a\n"))

test_derivedField :: Assertion
test_derivedField = do
  assertEqual
    "a missing key is null for aeson"
    (Right (Service "a" (ViaAeson Nothing)))
    (decodeText "name: a\n")
  assertEqual
    "the field"
    (Right (Service "a" (ViaAeson (Just (Server 80 "b")))))
    (decodeText "name: a\nbackend: {port: 80, host: b}\n")
  assertEqual
    "the output"
    "name: a\nbackend:\n  port: 80\n  host: b\n"
    (encodeText (Service "a" (ViaAeson (Just (Server 80 "b")))))

test_aesonTypes :: Assertion
test_aesonTypes = do
  assertEqual
    "a float for an Int"
    (Right (ViaAeson @Int 1))
    (decodeText "1.0")
  assertEqual
    "yes of YAML 1.2"
    (Right (ViaAeson @T.Text "yes"))
    (decodeText "yes")

-- | A decoder with a path that goes beyond a scalar.
newtype Nested = Nested Int
  deriving stock (Show)

instance A.FromJSON Nested where
  parseJSON v =
    Nested <$> A.parseJSON v A.<?> A.Key "inner" A.<?> A.Index 0 A.<?> A.Key "outer"

newtype Port = Port Int
  deriving stock (Show)

-- | The same message for every invalid value.
instance A.FromJSON Port where
  parseJSON v = Port <$> A.parseJSON v <|> fail "invalid port"

newtype Endpoint = Endpoint {port :: Port}
  deriving stock (Show, Generic)
  deriving anyclass (A.FromJSON)

data Service = Service {name :: T.Text, backend :: ViaAeson (Maybe Server)}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Service
