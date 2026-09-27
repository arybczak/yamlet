{-# OPTIONS_HADDOCK not-home #-}

-- | The values of the nodes of a syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.View
  ( View (..)
  , view
  , describeNode
  , isNullNode
  , stringValue
  ) where

import Data.Maybe
import Data.Text qualified as T

import Yamlet.Internal.Schema
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Node

-- | The value of a node with its tag resolved. The items and the entries of a
-- collection stay nodes of the syntax tree.
data View
  = ScalarView !Value
  | SequenceView [S.Node]
  | MappingView [(S.Node, S.Node)]
  | -- | 'Yamlet.Decode.runParser' replaces the aliases, so only a node that a
    -- program builds can have one.
    AliasView !T.Text

-- | The view of a node.
view :: S.Node -> View
view n = case n.content of
  S.Scalar style t -> ScalarView (scalarValue n.props.tag style t)
  S.Sequence _ xs -> SequenceView xs
  S.Mapping _ kvs -> MappingView kvs
  S.Alias name -> AliasView name
{-# INLINE view #-}

-- | The value of a scalar with the tag and the style.
scalarValue :: S.Tag -> S.ScalarStyle -> T.Text -> Value
scalarValue tag style t = case tag of
  S.NoTag
    | style == S.Plain -> resolvePlain t
    | otherwise -> String t
  S.NonSpecificTag -> String t
  -- 'Yamlet.Decode.runParser' rejects a value that is not valid for its tag.
  S.Tag tag' -> fromMaybe (String t) (resolveTagged tag' t)
{-# NOINLINE scalarValue #-}

-- | The kind of a node in plain words, e.g. "a list".
describeNode :: S.Node -> String
describeNode n = case view n of
  ScalarView v -> describe v
  SequenceView _ -> "a list"
  MappingView _ -> "a mapping"
  AliasView _ -> "an alias"

-- | The node is null.
isNullNode :: S.Node -> Bool
isNullNode n = case view n of
  ScalarView Null -> True
  _ -> False

-- | The text of a string node.
stringValue :: S.Node -> Maybe T.Text
stringValue n = case view n of
  ScalarView (String t) -> Just t
  _ -> Nothing
