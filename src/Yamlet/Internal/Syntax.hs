{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | The types of the syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Syntax
  ( -- * Documents
    Document (..)
  , YamlVersion (..)

    -- * Nodes
  , Node (..)
  , Content (.., ScalarContent)
  , Props (..)
  , noProps
  , Tag (..)
  , ScalarStyle (..)
  , isBlockScalar
  , CollectionStyle (..)

    -- * Comments
  , Comments (..)
  , noComments
  , withComments
  , Commented (..)
  , Line (.., Comment)

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

import Yamlet.Internal.Utils

-- | A document of a YAML stream.
data Document = Document
  { version :: !(Maybe YamlVersion)
  -- ^ The version from the @%YAML@ directive.
  , explicitStart :: !Bool
  -- ^ The document starts with a @---@ marker.
  , explicitEnd :: !Bool
  -- ^ The document ends with a @...@ marker.
  , docComments :: !Comments
  -- ^ The lines before the directives or the @---@ marker, the comment on the
  -- line of the marker and the lines at the end of the document: below the
  -- @...@ marker, or below a flow collection root.
  , root :: !Node
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The version of YAML that a document declares.
data YamlVersion = YamlVersion
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
    ScalarLinesContent !ScalarStyle !T.Text ![Int]
  | SequenceContent !CollectionStyle ![Node]
  | MappingContent !CollectionStyle ![(Node, Node)]
  | -- | An alias has no properties.
    AliasContent !T.Text
  deriving stock (Eq, Show, Generic)

-- | A scalar without positions of new lines. As a pattern, it matches every
-- scalar and ignores its positions.
pattern ScalarContent :: ScalarStyle -> T.Text -> Content
pattern ScalarContent style t <- ScalarLinesContent style t _
  where
    ScalarContent style t = ScalarLinesContent style t []

{-# COMPLETE ScalarContent, SequenceContent, MappingContent, AliasContent #-}

-- The instances of the sum types are written by hand, because GHC does not
-- always remove the generic representation of a sum type. A strict field of
-- a type without lazy parts, e.g. a text, is already in normal form.
instance NFData Content where
  rnf = \case
    ScalarLinesContent _ _ ls -> rnf ls
    SequenceContent _ xs -> rnf xs
    MappingContent _ kvs -> rnf kvs
    AliasContent _ -> ()

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
  | -- | A specific tag, e.g. @tag:yaml.org,2002:str@ for @!!str@. YAML has
    -- no syntax for the empty tag or a tag of one character, e.g. @x@ or
    -- @!@. The renderer writes such a tag as @!@, which reads back as
    -- 'NonSpecificTag'.
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

-- | The literal or the folded style.
isBlockScalar :: ScalarStyle -> Bool
isBlockScalar s = s == Literal || s == Folded

-- | The style of a collection: with indentation, or with brackets and commas.
data CollectionStyle
  = Block
  | Flow
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData CollectionStyle where
  rnf = rwhnf

-- | The comments and the empty lines that belong to a node.
data Comments = Comments
  { before :: ![Line]
  -- ^ The lines above the node.
  , inline :: !(Maybe T.Text)
  -- ^ The comment at the end of the first line of the node. The renderer
  -- writes a line break in it as a space, the same line breaks as in a
  -- 'Comment'.
  , after :: ![Line]
  -- ^ The lines after the last entry of a collection, between the brackets
  -- of an empty collection, or below a scalar or an alias root. The parser
  -- gives no such lines to other scalars and aliases, but the renderer
  -- writes them below the node.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | No comments and no empty lines.
noComments :: Comments
noComments = Comments [] Nothing []

-- | The node with the comments in place of its own. A record update of the
-- field is ambiguous where 'Commented' is in scope.
withComments :: Comments -> Node -> Node
withComments c n = Node n.offset n.endOffset n.props c n.content

-- | A value with the comments of its mapping entry:
--
-- * 'before': the lines above the entry,
-- * 'inline': the comment at the end of the first line of the entry,
-- * 'after': the lines after the value, e.g. after the last entry of a
--   collection.
--
-- A t'Commented' value of a mapping entry has the comments of the entry. A
-- block list or mapping under the key has its own comments: the lines below
-- the key up to the last empty line above its first entry. A t'Commented'
-- value inside the first one keeps them, so a type that keeps both nests two
-- t'Commented' values:
--
-- >>> input = "# The CI jobs.\njobs:\n  # Run on every push.\n\n  # Check the formatting.\n  - lint\n"
--
-- >>> T.putStr input
-- # The CI jobs.
-- jobs:
--   # Run on every push.
-- <BLANKLINE>
--   # Check the formatting.
--   - lint
--
-- >>> Right entries = decodeText @(M.Map T.Text (Commented (Commented [Commented T.Text]))) input
-- >>> Just jobs = M.lookup "jobs" entries
--
-- >>> jobs.comments
-- Comments {before = [Comment "The CI jobs."], inline = Nothing, after = []}
--
-- >>> jobs.value.comments
-- Comments {before = [Comment "Run on every push.",EmptyLine], inline = Nothing, after = []}
--
-- >>> map (.comments) jobs.value.value
-- [Comments {before = [Comment "Check the formatting."], inline = Nothing, after = []}]
--
-- A value without a key, e.g. an item of a list, has the comments of its
-- node. The comments of a list or a mapping stay with it, not with its first
-- item or key: the lines up to the last empty line above its first entry, and
-- the comment on its first line, e.g. after its tag. A t'Commented' value of
-- the whole list or mapping keeps them.
--
-- By the rules in [Comments]("Yamlet.Syntax#comments"), some lines read
-- back with a change:
--
-- * The lines after a text of several lines, which the encoder writes as a
--   block scalar, read back as the lines above the next entry. After the
--   last entry, they belong to the end of the collection around the entry.
-- * The lines above a list or a mapping without a key can get an empty line
--   below them, e.g. at the top level. The empty line reads back as the last
--   of these lines.
--
-- A comment survives only if its node decodes into a type with a place for
-- it, i.e. a node or a t'Commented' value. A key without a corresponding
-- Haskell field, e.g. the tag of a constructor, has no such type, so its
-- comments are lost. A comment at the end of a nested mapping survives only
-- if the field that holds the mapping is t'Commented', because a record has
-- no place for the end of its mapping. A record also has no place for the
-- comments of its mapping above its first key, so they are lost, e.g. a
-- comment at the top of a file above an empty line.
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
  { value :: !a
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
-- offset of the alias, i.e. of the place where the document uses the value.
--
-- Two equal values at different places are not equal as t'Located' values,
-- e.g. a set keeps both. The equality and the order compare the values first
-- and then the offsets. To compare only the values, e.g. in a test, use the
-- field @value@.
data Located a = Located
  { value :: !a
  , offset :: !Offset
  }
  -- The derived order compares the fields in this order.
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

-- | A line of comments.
data Line
  = EmptyLine
  | -- | The number of @#@ characters at the start of the comment, e.g. 2 for
    -- @## Section@, and the text after them and one space, without the white
    -- space at its end. The renderer writes a count below 1 as 1. It writes a
    -- text with line breaks as several comment lines with the same @#@
    -- characters, and the parser reads them back as several comments.
    -- U+0085, U+2028 and U+2029 count as line breaks here, because YAML 1.1
    -- reads them as line breaks.
    --
    -- The comment at the end of a line in t'Comments' is a text without a
    -- count. It keeps the @#@ characters after the first one in its text.
    CommentLine !Int !T.Text
  deriving stock (Eq, Ord, Generic)

-- | A comment with one @#@. As a pattern, it matches every comment and
-- ignores the number of @#@ characters.
pattern Comment :: T.Text -> Line
pattern Comment t <- CommentLine _ t
  where
    Comment t = CommentLine 1 t

{-# COMPLETE EmptyLine, Comment #-}

-- A comment with one @#@ shows as 'Comment', as a program usually writes it.
instance Show Line where
  showsPrec d = \case
    EmptyLine -> showString "EmptyLine"
    CommentLine 1 t -> showParen (d > 10) $ showString "Comment " . showsPrec 11 t
    CommentLine n t -> showParen (d > 10) $ showString "CommentLine " . showsPrec 11 n . showChar ' ' . showsPrec 11 t

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
        ScalarLinesContent style t ls -> ScalarLinesContent style (T.copy t) ls
        SequenceContent style xs -> SequenceContent style (strictMap copyNode xs)
        MappingContent style kvs -> MappingContent style (strictMap (\(k, v) -> strictPair (copyNode k) (copyNode v)) kvs)
        AliasContent name -> AliasContent (T.copy name)
    }

copyComments :: Comments -> Comments
copyComments c = case c of
  Comments [] Nothing [] -> c
  _ -> Comments {before = strictMap copyLine c.before, inline = copyMaybe c.inline, after = strictMap copyLine c.after}
  where
    copyLine :: Line -> Line
    copyLine = \case
      CommentLine n t -> CommentLine n (T.copy t)
      EmptyLine -> EmptyLine

-- | A copy without a thunk, which would keep the original text alive.
copyMaybe :: Maybe T.Text -> Maybe T.Text
copyMaybe = \case
  Just t -> Just $! T.copy t
  Nothing -> Nothing

-- $setup
-- >>> import Data.Map.Strict qualified as M
-- >>> import Data.Text.IO qualified as T
-- >>> import Yamlet
-- >>> printErrors = mapM_ (putStrLn . prettyError "input.yaml")
