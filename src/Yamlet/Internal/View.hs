{-# OPTIONS_HADDOCK not-home #-}

-- | The values of the nodes of a syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.View
  ( View (..)
  , view
  , scalarValue
  , describeNode
  , isNullNode
  , stringValue
  ) where

import Data.Maybe
import Data.Text qualified as T

import Yamlet.Internal.Schema
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Value

-- | The value of a node with its tag resolved. The items and the entries of a
-- collection stay nodes of the syntax tree. A tag that the schema does not
-- know does not matter, e.g. @!secret abc@ is a string.
data View
  = NullView
  | BoolView !Bool
  | IntView !Integer
  | FloatView !FloatValue
  | StringView !T.Text
  | SequenceView [S.Node]
  | MappingView [(S.Node, S.Node)]
  | -- | 'Yamlet.Decode.runParser' replaces the aliases, so only a node that a
    -- program builds can have one.
    AliasView !T.Text

-- | The view of a node.
view :: S.Node -> View
view n = case n.content of
  S.Scalar style t -> case scalarValue n.props.tag style t of
    Null -> NullView
    Bool b -> BoolView b
    Int i -> IntView i
    Float f -> FloatView f
    _ -> StringView t
  S.Sequence _ xs -> SequenceView xs
  S.Mapping _ kvs -> MappingView kvs
  S.Alias name -> AliasView name
{-# INLINE view #-}

-- | The value of a scalar with the tag and the style, without the tag.
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
describeNode n = case n.content of
  S.Scalar style t -> describe (scalarValue n.props.tag style t)
  S.Sequence _ _ -> "a list"
  S.Mapping _ _ -> "a mapping"
  S.Alias _ -> "an alias"

-- | The node is null.
isNullNode :: S.Node -> Bool
isNullNode n = case view n of
  NullView -> True
  _ -> False

-- | The text of a string node.
stringValue :: S.Node -> Maybe T.Text
stringValue n = case view n of
  StringView t -> Just t
  _ -> Nothing
