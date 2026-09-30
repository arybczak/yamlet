{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | The types of the syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Syntax
  ( -- * Documents
    Document (..)
  , Version (..)

    -- * Nodes
  , Node (..)
  , Content (.., Scalar)
  , Props (..)
  , noProps
  , Tag (..)
  , ScalarStyle (..)
  , CollectionStyle (..)

    -- * Comments
  , Comments (..)
  , noComments
  , Commented (..)
  , Line (..)

    -- * Positions
  , Offset (..)
  , noOffset
  , Located (..)

    -- * Copies
  , copyDocument
  , copyNode
  , copyComments
  ) where

import Control.DeepSeq
import Data.Text qualified as T
import GHC.Generics

-- | A document of a YAML stream.
data Document = Document
  { version :: !(Maybe Version)
  -- ^ The version from the @%YAML@ directive.
  , explicitStart :: !Bool
  -- ^ The document starts with a @---@ marker.
  , explicitEnd :: !Bool
  -- ^ The document ends with a @...@ marker.
  , docComments :: !Comments
  -- ^ The lines before the directives or the @---@ marker, the comment on the
  -- line of the marker and the lines at the end of the document.
  , root :: !Node
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The version of YAML that a document declares.
data Version = Version
  { major :: !Int
  , minor :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A node of a document.
data Node = Node
  { offset :: !Offset
  -- ^ The position of the first character of the content.
  , endOffset :: !Offset
  -- ^ The position after the last character of the content.
  , props :: !Props
  , comments :: !Comments
  , content :: !Content
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The content of a node.
data Content
  = -- | A scalar with the positions in its text where the source continues on
    -- a new line. A position counts the characters from the start of the
    -- text, and the positions are in ascending order. The parser gives them
    -- for the plain, quoted and folded styles, which join the lines of the
    -- source, so that the renderer can write the text on the same lines. The
    -- renderer ignores a position where the style of the output cannot start
    -- a new line and keep the text.
    ScalarLines !ScalarStyle !T.Text ![Int]
  | Sequence !CollectionStyle [Node]
  | Mapping !CollectionStyle [(Node, Node)]
  | -- | An alias has no properties.
    Alias !T.Text
  deriving stock (Eq, Show, Generic)

-- | A scalar without positions of new lines. As a pattern, it matches every
-- scalar and ignores its positions.
pattern Scalar :: ScalarStyle -> T.Text -> Content
pattern Scalar style t <- ScalarLines style t _
  where
    Scalar style t = ScalarLines style t []

{-# COMPLETE Scalar, Sequence, Mapping, Alias #-}

-- The instances of the sum types are written by hand, because GHC does not
-- always remove the generic representation of a sum type. A strict field of
-- a type without lazy parts, e.g. a text, is already in normal form.
instance NFData Content where
  rnf = \case
    ScalarLines _ _ ls -> rnf ls
    Sequence _ xs -> rnf xs
    Mapping _ kvs -> rnf kvs
    Alias _ -> ()

-- | The properties of a node.
data Props = Props
  { anchor :: !(Maybe T.Text)
  , tag :: !Tag
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | No anchor and no tag.
noProps :: Props
noProps = Props Nothing NoTag

-- | The tag of a node after the tag handles are expanded.
data Tag
  = -- | The node has no tag.
    NoTag
  | -- | The @!@ tag.
    NonSpecificTag
  | -- | A specific tag, e.g. @tag:yaml.org,2002:str@ for @!!str@.
    Tag !T.Text
  deriving stock (Eq, Ord, Show, Generic)

instance NFData Tag where
  rnf = rwhnf

-- | The style of a scalar: without quotes, in single or double quotes, or a
-- literal (@|@) or folded (@>@) block scalar.
data ScalarStyle
  = Plain
  | SingleQuoted
  | DoubleQuoted
  | Literal
  | Folded
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData ScalarStyle where
  rnf = rwhnf

-- | The style of a collection: with indentation, or with brackets and commas.
data CollectionStyle
  = Block
  | Flow
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData CollectionStyle where
  rnf = rwhnf

-- | The comments and the empty lines that belong to a node.
data Comments = Comments
  { before :: [Line]
  -- ^ The lines above the node.
  , inline :: !(Maybe T.Text)
  -- ^ The comment at the end of the first line of the node. The renderer
  -- writes a line break in it as a space, the same line breaks as in a
  -- 'Comment'.
  , after :: [Line]
  -- ^ The lines after the last entry of a collection, or between the brackets
  -- of an empty collection. The parser gives no such lines to a scalar or an
  -- alias, but the renderer writes them below it, e.g. the lines at the end
  -- of a document that 'Yamlet.decode' keeps at a root t'Node'.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | No comments and no empty lines.
noComments :: Comments
noComments = Comments [] Nothing []

-- | A value with the comments of its mapping entry:
--
-- * 'before': the lines above the entry,
-- * 'inline': the comment at the end of the first line of the entry,
-- * 'after': the lines after the value, e.g. after the last entry of a
--   collection.
--
-- The parser can give these comments to the key or to the value. The decoder
-- takes them from both and decodes the value without them. The lines above
-- the first entry of a block collection value stay inside the value.
--
-- A value without a key, e.g. an item of a list, has the comments of its
-- node. The decoder gives the lines above a list or a mapping to its first
-- item or key, so a list of t'Commented' values keeps a comment above its
-- first item. The comment on the first line of the list or the mapping, e.g.
-- after its tag, becomes one of these lines.
--
-- A comment survives only if its node decodes into a type with a place for
-- it, i.e. a node or a t'Commented' value. A key without a corresponding
-- Haskell field, e.g. the tag of a constructor, has no such type, so its
-- comments are lost. A comment at the end of a nested mapping survives only
-- if the field that holds the mapping is t'Commented', because a record has
-- no place for the end of its mapping.
--
-- The comments of the key are lost for a type such as
-- @data Name = Name (Commented Text)@ that derives its instances through
-- 'Generic'. A derived instance for one constructor with one field without a
-- name does not give the key of its entry to the value inside. Declare such
-- a type as a newtype and derive its instances with @deriving newtype@,
-- which gives the key to the value.
--
-- In a map, use t'Commented' on the key or on the value, not on both. With
-- both, the decoder gives the comments of the key to both, and the encoder
-- writes only those of the value, so a change to the comments of the key is
-- lost.
--
-- The order compares the values first and then the comments, e.g. in a set.
-- A change of the value keeps the comments:
--
-- >>> input = "# The port.\nport: 80 # the default\n"
--
-- >>> :{
-- either printErrors (T.putStr . encodeText . M.map (fmap (+ 1))) $
--   decodeText @(M.Map T.Text (Commented Int)) input
-- :}
-- # The port.
-- port: 81 # the default
data Commented a = Commented
  { value :: a
  , comments :: !Comments
  }
  -- The derived order compares the fields in this order.
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

-- | A value with the offset of its node, e.g. for the error of a check that
-- runs after the decode. The encoder writes only the value.
--
-- 'Yamlet.Error.documentErrors' turns the offsets into errors with lines,
-- columns and paths. It needs the text and the document of the decode, so
-- decode with 'Yamlet.decodeWithDocument'. Here the check gives an offset and
-- a message for each problem:
--
-- >>> input = "paths:\n- src\n- /etc\n"
--
-- >>> :{
-- case decodeWithDocument @(M.Map T.Text [Located T.Text]) input of
--   Right (config, doc) ->
--     let errs =
--           [ (p.offset, "the path is outside the repository")
--           | p <- concat (M.elems config)
--           , "/" `T.isPrefixOf` p.value
--           ]
--     in printErrors (documentErrors input doc errs)
--   Left errs -> printErrors errs
-- :}
-- input.yaml:3:3: paths[1]: the path is outside the repository
--   |
-- 3 | - /etc
--   |   ^
--
-- A value that no node gives, e.g. a value of 'Yamlet.Generic.yamlDefault',
-- has 'noOffset'. Its error has no position, and 'Yamlet.Error.prettyError'
-- prints only the file and the message. A value inside an alias has the
-- offset of the node with the anchor, because each alias is a copy of that
-- node.
--
-- Two equal values at different places are not equal as t'Located' values,
-- e.g. a set keeps both. The equality and the order compare the values first
-- and then the offsets. To compare only the values, e.g. in a test, use the
-- field @value@.
data Located a = Located
  { value :: a
  , offset :: !Offset
  }
  -- The derived order compares the fields in this order.
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

-- | A line of comments. Several empty lines in a row count as one.
data Line
  = EmptyLine
  | -- | The text after the @#@ and one space, without the white space at its
    -- end. The renderer writes a text with line breaks as several comment
    -- lines, and the parser reads them back as several comments. U+0085,
    -- U+2028 and U+2029 count as line breaks here, because YAML 1.1 reads
    -- them as line breaks.
    Comment !T.Text
  deriving stock (Eq, Ord, Show, Generic)

instance NFData Line where
  rnf = rwhnf

-- | The offset of a byte in the input text, in its UTF-8 encoding. For an
-- input in UTF-16 or UTF-32, the offset counts the bytes of the text after
-- 'Yamlet.Syntax.decodeInput', not the bytes of the input.
newtype Offset = Offset Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, NFData)

-- | The offset of a node that does not come from an input.
noOffset :: Offset
noOffset = Offset (-1)

-- | Copy every text of a document, so that the document does not keep the
-- input alive.
copyDocument :: Document -> Document
copyDocument doc =
  doc
    { docComments = copyComments doc.docComments
    , root = copyNode doc.root
    }

-- | Copy every text of a node, so that the node does not keep the input
-- alive.
copyNode :: Node -> Node
copyNode n =
  n
    { props = case n.props of
        -- Most nodes share one empty value, which a copy would duplicate.
        Props Nothing (Tag t) -> Props Nothing (Tag (T.copy t))
        Props Nothing _ -> n.props
        Props anchor tag ->
          Props
            { anchor = copyMaybe anchor
            , tag = case tag of
                Tag t -> Tag (T.copy t)
                t -> t
            }
    , comments = case n.comments of
        -- Most nodes share one empty value. GHC returns the result of
        -- 'copyComments' unboxed, so the caller would build a new one.
        c@(Comments [] Nothing []) -> c
        c -> copyComments c
    , content = case n.content of
        ScalarLines style t ls -> ScalarLines style (T.copy t) ls
        Sequence style xs -> Sequence style $! evaluated (map copyNode xs)
        Mapping style kvs -> Mapping style $! evaluated (map copyEntry kvs)
        Alias name -> Alias (T.copy name)
    }
  where
    copyEntry :: (Node, Node) -> (Node, Node)
    copyEntry (k, v) =
      let !k' = copyNode k
          !v' = copyNode v
      in (k', v')

copyComments :: Comments -> Comments
copyComments c = case c of
  Comments [] Nothing [] -> c
  _ ->
    let !before = evaluated (map copyLine c.before)
        !after = evaluated (map copyLine c.after)
    in Comments {before = before, inline = copyMaybe c.inline, after = after}
  where
    copyLine :: Line -> Line
    copyLine = \case
      Comment t -> Comment (T.copy t)
      EmptyLine -> EmptyLine

-- | A copy without a thunk, which would keep the original text alive.
copyMaybe :: Maybe T.Text -> Maybe T.Text
copyMaybe = \case
  Just t -> Just $! T.copy t
  Nothing -> Nothing

-- | The list with its spine and its elements evaluated.
evaluated :: [a] -> [a]
evaluated xs = foldr seq () xs `seq` xs

-- $setup
-- >>> import Data.Map.Strict qualified as M
-- >>> import Data.Text.IO qualified as T
-- >>> import Yamlet
-- >>> printErrors = mapM_ (putStrLn . prettyError "input.yaml")
