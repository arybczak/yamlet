{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Obligations for the inspection tests. They are in their own module,
-- because a splice cannot use a function of the module that holds it.
module Obligations
  ( hasNoGenericRep
  , assertSuccess
  ) where

import GHC.Generics qualified as G
import Language.Haskell.TH (Name)
import Test.Inspection
import Test.Tasty.HUnit

-- | The code uses no function and no constructor of the generic
-- representation. 'hasNoGenerics' checks the types instead, but the types
-- appear in coercions and in the types of join points after the optimizer
-- removed the representation.
hasNoGenericRep :: Name -> Obligation
hasNoGenericRep name =
  mkObligation name $
    NoUseOf
      [ 'G.from
      , 'G.to
      , '(G.:*:)
      , 'G.K1
      , 'G.L1
      , 'G.M1
      , 'G.R1
      , 'G.U1
      ]

-- | Fail with the Core of the function if the obligation does not hold.
assertSuccess :: Result -> Assertion
assertSuccess = \case
  Success _ -> pure ()
  Failure err -> assertFailure err
