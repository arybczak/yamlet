{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Obligations for the inspection tests. They are in their own module,
-- because a splice cannot use a function of the module that holds it.
module Obligations
  ( hasNoGenericRep
  , hasNoGenericDictionaries
  , assertSuccess
  , assertKnownFailure
  ) where

import GHC.Generics qualified as G
import Language.Haskell.TH
import Test.Inspection
import Test.Tasty.HUnit

import Yamlet

-- | The code uses no function and no constructor of the generic
-- representation. 'hasNoGenerics' checks the types instead, but the types
-- appear in coercions and in the types of join points after the optimizer
-- removed the representation. The constructors of the newtypes 'G.K1' and
-- 'G.M1' are casts in Core, so the list cannot name them.
hasNoGenericRep :: Name -> Obligation
hasNoGenericRep name =
  mkObligation name $
    NoUseOf
      [ 'G.from
      , 'G.to
      , '(G.:*:)
      , 'G.L1
      , 'G.R1
      , 'G.U1
      ]

-- | The code passes no dictionaries of the generic classes, e.g. to a method
-- of the instance for t'Yamlet.GenericYaml' that GHC did not inline at the
-- type. That method keeps the generic representation, and 'hasNoGenericRep'
-- does not see it, because it is in another module.
hasNoGenericDictionaries :: Name -> Obligation
hasNoGenericDictionaries name =
  mkObligation name $
    NoTypes
      [ ''G.Generic
      , ''GenericYamlOptions
      , ''GDatatype
      , ''GConstructors
      , ''GEncoding
      , ''GToConstructor
      , ''GFromConstructor
      , ''GFields
      , ''GToFields
      , ''GFromFields
      ]

-- | Fail with the Core of the function if the obligation does not hold.
assertSuccess :: Result -> Assertion
assertSuccess = \case
  Success _ -> pure ()
  Failure err -> assertFailure err

-- | Fail if the obligation holds, for a known failure. Then the test also
-- shows when a version of GHC fixes the failure.
assertKnownFailure :: Result -> Assertion
assertKnownFailure = \case
  Success msg -> assertFailure ("expected a failure, but " ++ msg)
  Failure _ -> pure ()
