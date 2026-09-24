{-# OPTIONS_HADDOCK not-home #-}
-- | Rendering of the syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Render
  ( RenderOptions(..)
  , defaultRenderOptions
  , Segment(..)
  , ExtraLine(..)
  , renderSyntax
  ) where

import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B

import Yamlet.Internal.Emit
import Yamlet.Internal.Syntax

-- | The options of 'renderSyntax'.
data RenderOptions = RenderOptions
  { extraLines :: [Segment] -> [ExtraLine]
  -- ^ The lines to write before the entry of a block collection at the
  -- given path. The empty path is the start of a document.
  , forceBlock :: Bool
  -- ^ Write every non-empty collection in the block style, unless it is in
  -- a flow collection.
  }

-- | No extra lines, and the collection styles of the tree.
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
  { extraLines = const []
  , forceBlock = False
  }

-- | A step of the path from the root of a document to an entry.
data Segment
  = Key T.Text
  -- ^ The entry of a mapping with a scalar key.
  | Index Int
  -- ^ The item of a sequence, or the entry of a mapping with another key.
  deriving stock (Eq, Ord, Show)

-- | A line before an entry.
data ExtraLine
  = EmptyLine
  | Comment T.Text
  -- ^ A comment. A line break in the text starts another comment line.
  deriving stock (Eq, Show)

-- | Render documents.
--
-- A scalar keeps its style if the style can hold its text, otherwise it gets
-- quotes. A block scalar never keeps trailing empty lines with the @+@
-- indicator, because the extra lines after it would become part of its
-- value. A collection in a flow collection or in a key keeps the flow style.
renderSyntax :: RenderOptions -> [Document] -> T.Text
renderSyntax opts = TL.toStrict . B.toLazyText . go True
  where
    go :: Bool -> [Document] -> B.Builder
    go afterEnd = \case
      [] -> mempty
      doc : docs -> document opts afterEnd doc <> go doc.explicitEnd docs

-- | A document. The flag tells if it starts the stream or follows a document
-- end marker.
document :: RenderOptions -> Bool -> Document -> B.Builder
document opts afterEnd doc = mconcat
  [ extras opts 0 []
  , case doc.version of
      Just v -> (if afterEnd then mempty else "...\n")
        <> "%YAML " <> B.fromString (show v.major) <> "." <> B.fromString (show v.minor) <> "\n"
      Nothing -> mempty
  , body
  , if doc.explicitEnd then "...\n" else mempty
  ]
  where
    -- A document needs a start marker after another document, after
    -- directives, and if it is empty.
    marker :: Bool
    marker = doc.explicitStart || isJust doc.version || not afterEnd || isEmpty doc.root

    body :: B.Builder
    body
      | isBlock opts doc.root = mconcat
          [ if marker then "---" <> maybe mempty (" " <>) (props doc.root) <> "\n"
            else maybe mempty (<> "\n") (props doc.root)
          , block opts 0 True [] doc.root
          ]
      | isEmpty doc.root = "---\n"
      | marker = "--- " <> inline opts InValue 2 doc.root <> "\n"
      | otherwise = inline opts InValue 2 doc.root <> "\n"

-- | A block collection without its properties, at the given indentation. The
-- first entry does not start with indentation if the collection continues a
-- line.
block :: RenderOptions -> Int -> Bool -> [Segment] -> Node -> B.Builder
block opts indent atLineStart path = \case
  Sequence _ _ _ xs -> mconcat $ zipWith item [0 ..] xs
  Mapping _ _ _ kvs -> mconcat $ zipWith entry [0 ..] kvs
  _ -> mempty
  where
    start :: Int -> [Segment] -> B.Builder
    start i path'
      | i == 0 && not atLineStart = mempty
      | otherwise = extras opts indent path' <> spaces indent

    item :: Int -> Node -> B.Builder
    item i x = let path' = Index i : path
               in start i path' <> "-" <> after opts indent path' x

    entry :: Int -> (Node, Node) -> B.Builder
    entry i (k, v) = let path' = segment i k : path
                     in start i path' <> case implicitKey opts k of
                          Just key -> key <> ":" <> value opts indent path' v
                          Nothing -> "?" <> after opts indent path' k
                            <> spaces indent <> ":" <> after opts indent path' v

-- | The value of a mapping entry after the colon, with the line break.
value :: RenderOptions -> Int -> [Segment] -> Node -> B.Builder
value opts indent path n
  | isBlock opts n = case n of
      Sequence{} -> header <> block opts indent True path n
      _ -> header <> block opts (indent + 2) True path n
  | isEmpty n = "\n"
  | otherwise = " " <> inline opts InValue (indent + 2) n <> "\n"
  where
    header :: B.Builder
    header = maybe mempty (" " <>) (props n) <> "\n"

-- | A node after the indicator of a sequence item or an explicit entry, with
-- the line break. A block collection starts on the same line if it can.
after :: RenderOptions -> Int -> [Segment] -> Node -> B.Builder
after opts indent path n
  | isBlock opts n = case props n of
      Nothing | null (extras' firstPath) -> " " <> block opts (indent + 2) False path n
      p -> maybe mempty (" " <>) p <> "\n" <> block opts (indent + 2) True path n
  | isEmpty n = "\n"
  | otherwise = " " <> inline opts InValue (indent + 2) n <> "\n"
  where
    firstPath :: [Segment]
    firstPath = case n of
      Mapping _ _ _ ((k, _) : _) -> segment 0 k : path
      _ -> Index 0 : path

    extras' :: [Segment] -> [ExtraLine]
    extras' = opts.extraLines . reverse

-- | Where an inline node is.
data Position = InValue | InKey | InFlow
  deriving stock Eq

-- | A node on one line, except a block scalar, whose content lines are at the
-- given indentation.
inline :: RenderOptions -> Position -> Int -> Node -> B.Builder
inline opts pos indent n = case n of
  Alias _ name -> "*" <> B.fromText name
  _ -> case props n of
    Just p | isEmpty' -> p
           | otherwise -> p <> " " <> content
    Nothing -> content
  where
    isEmpty' :: Bool
    isEmpty' = case n of
      Scalar _ _ Plain t -> T.null t
      _ -> False

    content :: B.Builder
    content = case n of
      Scalar _ _ style t -> scalar pos indent style t
      Sequence _ _ _ xs -> "[" <> commas (map (inline opts InFlow indent . flowItem) xs) <> "]"
      Mapping _ _ _ kvs -> "{" <> commas (map flowEntry kvs) <> "}"
      Alias{} -> mempty

    -- An empty scalar cannot be an item of a flow sequence.
    flowItem :: Node -> Node
    flowItem = \case
      Scalar off (Props anchor NoTag) Plain "" | isNothing anchor ->
        Scalar off (Props Nothing (Tag "tag:yaml.org,2002:null")) Plain ""
      x -> x

    flowEntry :: (Node, Node) -> B.Builder
    flowEntry (k, v) = mconcat
      [ inline opts InFlow indent k
      , if endsWithName k then " :" else ":"
      , if isEmpty v then mempty else " " <> inline opts InFlow indent v
      ]

    commas :: [B.Builder] -> B.Builder
    commas = \case
      [] -> mempty
      b : bs -> b <> mconcat (map (", " <>) bs)

-- | A scalar in its style, or in a style that can hold its text.
scalar :: Position -> Int -> ScalarStyle -> T.Text -> B.Builder
scalar pos indent style t = case style of
  Plain
    | T.null t -> mempty
    | plainSyntax (pos == InFlow) t -> B.fromText t
    | otherwise -> quoted
  SingleQuoted -> quoted
  DoubleQuoted -> doubleQuoted t
  Literal | pos == InValue -> fromMaybe (doubleQuoted t) (literalBlock False indent t)
  Folded | pos == InValue -> fromMaybe (doubleQuoted t) (foldedBlock indent t)
  _ -> doubleQuoted t
  where
    quoted :: B.Builder
    quoted = fromMaybe (doubleQuoted t) (singleQuoted t)

-- | A key on one line, or 'Nothing' if it needs an explicit entry.
implicitKey :: RenderOptions -> Node -> Maybe B.Builder
implicitKey opts k
  | isBlock opts k = Nothing
  | isEmpty k = Nothing
  | TL.length (B.toLazyText key) > 1024 = Nothing
  | otherwise = Just key
  where
    key :: B.Builder
    key = inline opts InKey 0 k <> if endsWithName k then " " else mempty

-- | The segment of the path for the entry with the given index and key.
segment :: Int -> Node -> Segment
segment i = \case
  Scalar _ _ _ t -> Key t
  _ -> Index i

-- | The node is a collection that the renderer writes in the block style.
isBlock :: RenderOptions -> Node -> Bool
isBlock opts = \case
  Sequence _ _ style (_ : _) -> style == Block || opts.forceBlock
  Mapping _ _ style (_ : _) -> style == Block || opts.forceBlock
  _ -> False

-- | The node is an empty plain scalar without properties.
isEmpty :: Node -> Bool
isEmpty = \case
  Scalar _ (Props Nothing NoTag) Plain t -> T.null t
  _ -> False

-- | The node ends with an alias, an anchor or a tag. A colon right after it
-- would be part of the name.
endsWithName :: Node -> Bool
endsWithName = \case
  Alias{} -> True
  Scalar _ p Plain t -> T.null t && (isJust p.anchor || p.tag /= NoTag)
  _ -> False

-- | The anchor and the tag of a node.
props :: Node -> Maybe B.Builder
props n = case n of
  Scalar _ p _ _ -> render p
  Sequence _ p _ _ -> render p
  Mapping _ p _ _ -> render p
  Alias{} -> Nothing
  where
    render :: Props -> Maybe B.Builder
    render p = case (anchor, tag) of
      (Nothing, Nothing) -> Nothing
      (Just a, Nothing) -> Just a
      (Nothing, Just t) -> Just t
      (Just a, Just t) -> Just (a <> " " <> t)
      where
        anchor :: Maybe B.Builder
        anchor = ("&" <>) . B.fromText <$> p.anchor

        tag :: Maybe B.Builder
        tag = case p.tag of
          NoTag -> Nothing
          NonSpecificTag -> Just "!"
          Tag t -> Just (tagText t)

-- | The extra lines before the entry at the path, at the given indentation.
extras :: RenderOptions -> Int -> [Segment] -> B.Builder
extras opts indent path = mconcat . map extraLine $ opts.extraLines (reverse path)
  where
    extraLine :: ExtraLine -> B.Builder
    extraLine = \case
      EmptyLine -> "\n"
      Comment t -> mconcat . map comment $ T.splitOn "\n" (T.replace "\r" "\n" (T.replace "\r\n" "\n" t))

    comment :: T.Text -> B.Builder
    comment l
      | T.null l = spaces indent <> "#\n"
      | otherwise = spaces indent <> "# " <> B.fromText l <> "\n"
