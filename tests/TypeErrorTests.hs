{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | The type errors of the generic instances. The module defers type errors,
-- so an instance with a type error compiles, and using it throws the error.
module TypeErrorTests (typeErrorTests) where

import Control.Exception
import Data.List qualified as L
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet

typeErrorTests :: TestTree
typeErrorTests =
  testGroup
    "Type errors"
    [ testCase "several fields without names" $ do
        rejects "The constructor Pair has several fields without names." (encodeText (Pair 1 "a"))
        rejects "The constructor Pair has several fields without names." (decodeText @Pair "[1, a]")
    , testCase "several fields without names in a sum" $
        rejects "The constructor Line has several fields without names." (encodeText (Line 1 2))
    , testCase "named fields and a field without a name" $
        rejects
          "The constructor Circle has named fields and the constructor Label has one field without a name."
          (encodeText (Label "x"))
    , testCase "flat named fields" $
        rejects
          "TaggedFlat needs constructors with one field without a name, but the constructor Jump has named fields."
          (encodeText (Jump 1))
    , testCase "several fields without names in a single field" $
        rejects "The constructor Coords has several fields without names." (encodeText (Coords 1 2))
    , testCase "no constructors" $
        rejects "A type without constructors cannot derive FromYaml or ToYaml" (decodeText @Empty "null")
    ]

data Pair = Pair Int T.Text
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Pair

data Segment = Line Double Double | Dot
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Segment

data Mixed = Circle {radius :: Double} | Label T.Text
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Mixed

data FlatNamed = Jump {height :: Int} | Halt
  deriving stock (Generic)
  deriving (FromYaml, ToYaml) via GenericYaml FlatNamed

instance GenericYamlOptions FlatNamed where
  type SumEncoding FlatNamed = TaggedFlat

data Place = Coords Double Double | Nowhere
  deriving stock (Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Place

instance GenericYamlOptions Place where
  type SumEncoding Place = SingleField

data Empty
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Empty

-- | Using the value throws a deferred type error with the message.
rejects :: String -> a -> Assertion
rejects expected x =
  try (evaluate x) >>= \case
    Left (TypeError msg) ->
      assertBool ("the message contains " ++ show expected ++ ":\n" ++ msg) (expected `L.isInfixOf` msg)
    Right _ -> assertFailure "expected a type error"
