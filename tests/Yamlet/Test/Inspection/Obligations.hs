{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Obligations for the inspection tests. They are in their own module,
-- because a splice cannot use a function of the module that holds it.
--
-- Keep every function of this module, also one that no test uses at the
-- moment, e.g. 'assertFailureIf' and 'ghcVersion' when no test expects a
-- failure. A later change to the library or a new version of GHC can need
-- them again.
module Yamlet.Test.Inspection.Obligations
  ( hasNoGenericRep
  , hasNoGenericDictionaries
  , assertSuccess
  , assertFailureIf
  , ghcVersion
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

-- | If the flag is set, fail if the obligation holds, for a known failure,
-- e.g. on a version of GHC that optimizes the code less. Then the test also
-- shows when a version of GHC fixes the failure. Otherwise, 'assertSuccess'.
assertFailureIf :: Bool -> Result -> Assertion
assertFailureIf = \case
  True -> \case
    Success msg -> assertFailure ("expected a failure, but " ++ msg)
    Failure _ -> pure ()
  False -> assertSuccess

-- | The major version of GHC, e.g. @(9, 12)@.
ghcVersion :: (Int, Int)
ghcVersion = __GLASGOW_HASKELL__ `quotRem` 100
