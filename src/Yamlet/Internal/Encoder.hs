{-# OPTIONS_HADDOCK not-home #-}

-- | The renderer of the nodes of the encoder.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Encoder
  ( renderDocuments
  ) where

import Data.Containers.ListUtils
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as B

import Yamlet.Internal.Emit
import Yamlet.Internal.Utils
import Yamlet.Syntax qualified as S

-- | Render documents. Documents after the first one start with a @---@
-- marker. The collections of 'Yamlet.Encode.toYaml' are in the block style.
--
-- A document with comments, anchors, aliases, flow collections, scalars on
-- several lines or scalar styles that 'Yamlet.Encode.toYaml' does not create
-- goes to
-- 'Yamlet.Syntax.renderSyntax'. Other documents go to a faster renderer,
-- which gives the same output.
renderDocuments :: [S.Node] -> T.Text
renderDocuments docs
  | all simple docs = B.runBuilder . mconcat $ zipWith document [0 :: Int ..] docs
  | otherwise = S.renderSyntax S.defaultRenderOptions (map S.document docs)
  where
    document :: Int -> S.Node -> B.Builder
    document i n
      | null handles = (if i > 0 then "---\n" else mempty) <> topLevel n
      | otherwise = (if i > 0 then "...\n" else mempty) <> foldMap tagDirective handles <> "---\n" <> topLevel n
      where
        -- The handles for the tags that are not valid URIs.
        handles :: [Char]
        handles = nubOrd $ tagHandles n []

    tagHandles :: S.Node -> [Char] -> [Char]
    tagHandles n acc =
      (case n.props.tag of S.Tag t -> maybe id (:) (tagHandle t); _ -> id) $ case n.content of
        S.Sequence _ xs -> foldr tagHandles acc xs
        S.Mapping _ kvs -> foldr (\(k, v) -> tagHandles k . tagHandles v) acc kvs
        _ -> acc

    topLevel :: S.Node -> B.Builder
    topLevel n = case n.content of
      S.Sequence _ (_ : _) -> tagLine n <> blockSequence 0 True n
      S.Mapping _ (_ : _) -> tagLine n <> blockMapping 0 True n
      S.Scalar S.Literal t | needsIndentIndicator t -> withTag n (doubleQuoted t) <> "\n"
      _ -> inlineValue indentStep n <> "\n"

    -- A tag of a block collection takes a line of its own.
    tagLine :: S.Node -> B.Builder
    tagLine n = case tagPrefix n of
      Just t -> t <> "\n"
      Nothing -> mempty

-- | The node has no comments, anchors, aliases and flow collections, and its
-- scalars are on one line and have the styles that 'Yamlet.Encode.toYaml'
-- creates.
simple :: S.Node -> Bool
simple n =
  null n.comments.before
    && isNothing n.comments.inline
    && null n.comments.after
    && isNothing n.props.anchor
    && n.props.tag /= S.NonSpecificTag
    && case n.content of
      S.ScalarLines _ _ (_ : _) -> False
      -- The renderer gives an empty plain scalar no text.
      S.Scalar S.Plain t -> not (T.null t)
      S.Scalar S.SingleQuoted _ -> True
      S.Scalar S.DoubleQuoted _ -> True
      S.Scalar S.Literal _ -> True
      S.Scalar _ _ -> False
      S.Sequence style xs -> (style == S.Block || null xs) && all simple xs
      S.Mapping style kvs -> (style == S.Block || null kvs) && all (\(k, v) -> simple k && simple v) kvs
      S.Alias _ -> False

-- | A block sequence of a 'simple' node. The first entry does not start with
-- indentation if the sequence continues a line.
blockSequence :: Int -> Bool -> S.Node -> B.Builder
blockSequence indent atLineStart n = case n.content of
  S.Sequence _ xs -> mconcat $ zipWith entry [0 :: Int ..] xs
  _ -> mempty
  where
    entry :: Int -> S.Node -> B.Builder
    entry i x = (if i > 0 || atLineStart then spaces indent else mempty) <> "-" <> afterIndicator indent x

-- | A node after the indicator of a sequence item or an explicit entry at the
-- given indentation, with the line break. A block collection starts on the
-- line of the indicator, unless it has a tag.
afterIndicator :: Int -> S.Node -> B.Builder
afterIndicator indent x = case x.content of
  S.Sequence _ (_ : _) -> collection $ blockSequence (indent + indentStep) False x
  S.Mapping _ (_ : _) -> collection $ blockMapping (indent + indentStep) False x
  _ -> " " <> inlineValue (indent + indentStep) x <> "\n"
  where
    collection :: B.Builder -> B.Builder
    collection body = case tagPrefix x of
      Just t -> " " <> t <> "\n" <> spaces (indent + indentStep) <> body
      Nothing -> " " <> body

-- | A block mapping of a 'simple' node. The first entry does not start with
-- indentation if the mapping continues a line.
blockMapping :: Int -> Bool -> S.Node -> B.Builder
blockMapping indent atLineStart n = case n.content of
  S.Mapping _ kvs -> mconcat $ zipWith entry [0 :: Int ..] kvs
  _ -> mempty
  where
    entry :: Int -> (S.Node, S.Node) -> B.Builder
    entry i (k, v) =
      (if i > 0 || atLineStart then spaces indent else mempty) <> case implicitKey k of
        Just key -> key <> ":" <> value v
        Nothing -> "?" <> afterIndicator indent k <> spaces indent <> ":" <> afterIndicator indent v

    value :: S.Node -> B.Builder
    value v = case v.content of
      S.Sequence _ (_ : _) -> tagged v <> "\n" <> blockSequence indent True v
      S.Mapping _ (_ : _) -> tagged v <> "\n" <> blockMapping (indent + indentStep) True v
      _ -> " " <> inlineValue (indent + indentStep) v <> "\n"

    tagged :: S.Node -> B.Builder
    tagged x = maybe mempty (" " <>) (tagPrefix x)

-- | A key that fits on one line, or 'Nothing' if it needs an explicit entry.
implicitKey :: S.Node -> Maybe B.Builder
implicitKey k = case k.content of
  S.Scalar style t
    | S.NoTag <- k.props.tag
    , style == S.Plain
    , plainSyntax False t ->
        if T.length t > maxImplicitKeyLength then Nothing else Just (B.fromText t)
    | otherwise -> fits (withTag k (scalarText style t))
  S.Sequence _ [] -> fits (inlineValue 0 k)
  S.Mapping _ [] -> fits (inlineValue 0 k)
  _ -> Nothing
  where
    fits :: B.Builder -> Maybe B.Builder
    fits key = if T.length (B.runBuilder key) > maxImplicitKeyLength then Nothing else Just key

-- | A scalar, or an empty collection in the flow style.
inlineValue :: Int -> S.Node -> B.Builder
inlineValue indent n = withTag n $ case n.content of
  S.Sequence _ _ -> "[]"
  S.Mapping _ _ -> "{}"
  S.Scalar S.Literal t | Just (h, b) <- literalBlock True indent t -> h <> b
  S.Scalar style t -> scalarText style t
  S.Alias _ -> mempty

-- | Prefix the tag if the node has one.
withTag :: S.Node -> B.Builder -> B.Builder
withTag n b = case tagPrefix n of
  Just t -> t <> " " <> b
  Nothing -> b

tagPrefix :: S.Node -> Maybe B.Builder
tagPrefix n = case n.props.tag of
  S.Tag t -> Just (tagText t)
  _ -> Nothing

-- | A scalar of a 'simple' node on one line.
scalarText :: S.ScalarStyle -> T.Text -> B.Builder
scalarText style t = case style of
  S.Plain
    | plainSyntax False t -> B.fromText t
    | otherwise -> quotedPlain t
  S.SingleQuoted -> quoted
  _ -> doubleQuoted t
  where
    quoted :: B.Builder
    quoted = fromMaybe (doubleQuoted t) (singleQuoted t)
