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
import GHC.Generics

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
  | SequenceView ![S.Node]
  | MappingView ![(S.Node, S.Node)]
  | -- | 'Yamlet.Decode.runParser' replaces the aliases, so only a node that a
    -- program builds can have one.
    AliasView !T.Text
  deriving stock (Generic)

-- | The view of a node.
view :: S.Node -> View
view n = case n.content of
  S.ScalarContent style t -> case scalarValue n.props.tag style t of
    Null -> NullView
    Bool b -> BoolView b
    Int i -> IntView i
    Float f -> FloatView f
    _ -> StringView t
  S.SequenceContent _ xs -> SequenceView xs
  S.MappingContent _ kvs -> MappingView kvs
  S.AliasContent name -> AliasView name
-- GHC does not inline it without the pragma. Inlined, a match on the view
-- allocates no view. Without it, the decode benchmarks and the parseYaml
-- benchmarks of the derived instances allocated more.
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
-- Inlined, it saves little of the allocation of a decoder in the decode
-- benchmarks, but each match on 'view' gets a copy of it, and a small
-- instance grows much.
{-# NOINLINE scalarValue #-}

-- | The kind of a node in plain words, e.g. "a list".
--
-- >>> map describeNode <$> decodeText @[Node] "- [1, 2]\n- 3.5\n- ~\n- !!str 12\n"
-- Right ["a list","a floating-point number","null","a string"]
describeNode :: S.Node -> String
describeNode n = case n.content of
  S.ScalarContent style t -> describe (scalarValue n.props.tag style t)
  S.SequenceContent _ _ -> "a list"
  S.MappingContent _ _ -> "a mapping"
  S.AliasContent _ -> "an alias"

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

-- $setup
-- >>> import Yamlet
