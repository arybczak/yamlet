-- | The representation of a YAML stream that keeps every detail of the
-- presentation: the styles of scalars and collections, the lines of
-- scalars, anchors, aliases, unresolved tags, comments and empty lines. The
-- t'Yamlet.Decode.FromYaml' and t'Yamlet.Encode.ToYaml' classes read and
-- write the nodes of this tree.
--
-- Most texts in the tree share the memory of the input, so a node keeps the
-- whole input alive. To keep a text longer than the tree, copy it with
-- 'Data.Text.copy', or copy the whole tree with 'copyDocument'. Copy only
-- what the program keeps: a copy of a whole tree usually needs more memory
-- than the input it frees.
--
-- A document that the renderer writes back keeps its comments, empty lines
-- and styles:
--
-- >>> input = "# The server.\nhost: localhost # only local\n\nports: [80, 443]\n"
--
-- >>> :{
-- case parseDocumentsText input of
--   Left err -> putStrLn (prettyError "input.yaml" err)
--   Right docs -> T.putStr (renderSyntax defaultRenderOptions docs)
-- :}
-- # The server.
-- host: localhost # only local
-- <BLANKLINE>
-- ports: [80, 443]
--
-- The section [Comments]("Yamlet.Syntax#comments") gives the rules that
-- decide the node of each comment.
module Yamlet.Syntax
  ( -- * Parsing
    parseDocuments
  , parseDocumentsText
  , decodeInput
  , copyDocument
  , copyNode

    -- * Rendering
  , renderSyntax
  , RenderOptions (..)
  , defaultRenderOptions

    -- * Documents
  , Document (..)
  , Version (..)
  , document

    -- * Nodes
  , Node (..)
  , Content (..)
  , Props (..)
  , noProps
  , Tag (..)
  , ScalarStyle (..)
  , CollectionStyle (..)

    -- ** Construction
  , contentNode
  , scalarNode
  , plainNode
  , foldedNode
  , sequenceNode
  , mappingNode

    -- * Positions
  , Offset (..)
  , noOffset

    -- * Comments
    -- $comments
  , Comments (..)
  , noComments
  , Line (..)

    -- ** Lines above a node
    -- $linesAbove

    -- ** Comments at the end of a line
    -- $endOfLine

    -- ** Lines at the end of a collection
    -- $endOfCollection

    -- ** Documents
    -- $documents

    -- ** Empty lines
    -- $emptyLines
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T

import Yamlet.Error
import Yamlet.Internal.Input
import Yamlet.Internal.Parser
import Yamlet.Internal.Render
import Yamlet.Internal.Syntax

-- | Parse the documents of a stream. The encoding is UTF-8, UTF-16 or UTF-32,
-- detected as the YAML specification describes.
--
-- 'errorAt' and 'Yamlet.decodeDocument' need the text of the input. To report
-- errors with the lines of the input, decode the input with 'decodeInput' and
-- parse it with 'parseDocumentsText'.
parseDocuments :: BS.ByteString -> Either Error [Document]
parseDocuments bs = decodeInput bs >>= parseStream

-- | Parse the documents of a stream.
--
-- >>> length <$> parseDocumentsText "a\n---\nb\n"
-- Right 2
parseDocumentsText :: T.Text -> Either Error [Document]
parseDocumentsText = parseStream

-- | A document with the given root, without directives, markers and comments.
document :: Node -> Document
document n =
  Document
    { version = Nothing
    , explicitStart = False
    , explicitEnd = False
    , docComments = noComments
    , root = n
    }

-- | A node with the given content, without properties and comments.
contentNode :: Content -> Node
contentNode c =
  Node
    { offset = noOffset
    , endOffset = noOffset
    , props = noProps
    , comments = noComments
    , content = c
    }

-- | A scalar in the given style. If the style cannot hold the text,
-- 'renderSyntax' uses quotes.
--
-- >>> T.putStr (renderSyntax defaultRenderOptions [document (mappingNode [(plainNode "key", scalarNode Plain "a: b")])])
-- key: 'a: b'
scalarNode :: ScalarStyle -> T.Text -> Node
scalarNode style = contentNode . Scalar style

-- | A plain scalar.
plainNode :: T.Text -> Node
plainNode = scalarNode Plain

-- | A folded block scalar (@>-@) with the given lines.
--
-- >>> T.putStr (renderSyntax defaultRenderOptions [document (mappingNode [(plainNode "options", foldedNode ["--health-cmd pg_isready", "--health-interval 5s"])])])
-- options: >-
--   --health-cmd pg_isready
--   --health-interval 5s
foldedNode :: [T.Text] -> Node
foldedNode ls = contentNode (ScalarLines Folded t starts)
  where
    (t, starts) = foldedText (contentLines 0 ls)

    -- The lines with content, each with the number of empty lines above it.
    contentLines :: Int -> [T.Text] -> [BlockLine]
    contentLines !empties = \case
      [] -> []
      l : rest
        | T.null l -> contentLines (empties + 1) rest
        | otherwise -> BlockLine empties l : contentLines 0 rest

-- | A block sequence.
sequenceNode :: [Node] -> Node
sequenceNode = contentNode . Sequence Block

-- | A block mapping.
mappingNode :: [(Node, Node)] -> Node
mappingNode = contentNode . Mapping Block

-- $comments
-- #comments#
-- The parser gives each comment to one node or document, and the renderer
-- writes it back at that place. A stream without documents, e.g. a stream of
-- only comments, has no such place. The parser drops its comments.
--
-- In the examples below, @printComments@ parses a text and prints each node
-- that has comments, with its path and the fields of t'Comments'. The key and
-- the value of an entry have the same path, with @(key)@ or @(value)@ after
-- it.

-- $linesAbove
-- A comment on a line of its own belongs to the node below it. Above the
-- first entry of a block collection, the lines up to the last empty line
-- belong to the collection, e.g. a comment at the top of a file.
--
-- >>> input = "# The server.\n\n# The host.\nhost: localhost\n# The port.\nport: 80\n"
--
-- >>> T.putStr input
-- # The server.
-- <BLANKLINE>
-- # The host.
-- host: localhost
-- # The port.
-- port: 80
--
-- >>> printComments input
-- root before: [Comment "The server.",EmptyLine]
-- root.host (key) before: [Comment "The host."]
-- root.port (key) before: [Comment "The port."]
--
-- A block collection after @- @ on the same line keeps all the lines above
-- it, so that a comment above an item belongs to the item.
--
-- >>> input = "# The first server.\n- host: localhost\n# The second server.\n- host: example.com\n"
--
-- >>> T.putStr input
-- # The first server.
-- - host: localhost
-- # The second server.
-- - host: example.com
--
-- >>> printComments input
-- root[0] before: [Comment "The first server."]
-- root[1] before: [Comment "The second server."]
--
-- The rules in the sections below give some of these lines to a document or
-- to the end of a collection instead, e.g. @# a@ and @# b@ below. The node
-- below still gets @# c@.
--
-- >>> input = "# a\n---\nserver:\n  host: localhost\n  # b\n# c\nuser: admin\n"
--
-- >>> T.putStr input
-- # a
-- ---
-- server:
--   host: localhost
--   # b
-- # c
-- user: admin
--
-- >>> printComments input
-- document before: [Comment "a"]
-- root.server (value) after: [Comment "b"]
-- root.user (key) before: [Comment "c"]

-- $endOfLine
-- A comment at the end of a line belongs to the node that ends last before
-- it on that line, if only spaces, a colon or a comma come between them.
-- E.g. the value gets the comment in @key: value # comment@, and the key
-- gets it in @key: # comment@. A comment on the line of a block scalar
-- header belongs to the block scalar.
--
-- >>> input = "host: localhost # a\nports: # b\n- 80\ntext: | # c\n  Hello.\n"
--
-- >>> T.putStr input
-- host: localhost # a
-- ports: # b
-- - 80
-- text: | # c
--   Hello.
--
-- >>> printComments input
-- root.host (value) inline: "a"
-- root.ports (key) inline: "b"
-- root.text (value) inline: "c"
--
-- A comment at the end of a line that the rule above does not give to a
-- node, e.g. after @- @, belongs to the node below it. If that node also has
-- a comment at the end of its line, the first comment becomes a line above
-- the node, e.g. @# c@ below.
--
-- >>> input = "- # a\n  host: localhost # b\n- # c\n  'a string' # d\n"
--
-- >>> T.putStr input
-- - # a
--   host: localhost # b
-- - # c
--   'a string' # d
--
-- >>> printComments input
-- root[0] inline: "a"
-- root[0].host (value) inline: "b"
-- root[1] before: [Comment "c"]
-- root[1] inline: "d"
--
-- The same holds for a comment after the tag of a block collection.
--
-- >>> input = "server: !!map # a\n  host: localhost\n"
--
-- >>> T.putStr input
-- server: !!map # a
--   host: localhost
--
-- >>> printComments input
-- root.server (value) inline: "a"
--
-- On the line of the @---@ marker, the document gets such a comment.
--
-- >>> input = "--- !!map # a\nhost: localhost\n"
--
-- >>> T.putStr input
-- --- !!map # a
-- host: localhost
--
-- >>> printComments input
-- document inline: "a"

-- $endOfCollection
-- A comment below a scalar or an alias in a block collection belongs to the
-- end of that node if it is indented deeper than the key or the @-@ of its
-- entry. Below the last item of a list without indentation, it belongs to
-- the end of the list, by the rule below.
--
-- >>> input = "host: localhost\n  # a\n# b\nports:\n- 80\n  # c\n# d\n- 443\n  # e\n# f\nuser: admin\n"
--
-- >>> T.putStr input
-- host: localhost
--   # a
-- # b
-- ports:
-- - 80
--   # c
-- # d
-- - 443
--   # e
-- # f
-- user: admin
--
-- >>> printComments input
-- root.host (value) after: [Comment "a"]
-- root.ports (key) before: [Comment "b"]
-- root.ports (value) after: [Comment "e"]
-- root.ports[0] after: [Comment "c"]
-- root.ports[1] before: [Comment "d"]
-- root.user (key) before: [Comment "f"]
--
-- Below a block scalar, such a line is part of the scalar if it is indented
-- as deep as the content. Otherwise it belongs to the node below. E.g.
-- @# a@ below is a line of the text, and @user@ gets @# b@.
--
-- >>> input = "text: |\n    Hello.\n    # a\n  # b\nuser: admin\n"
--
-- >>> T.putStr input
-- text: |
--     Hello.
--     # a
--   # b
-- user: admin
--
-- >>> printComments input
-- root.user (key) before: [Comment "b"]
--
-- A comment after the last entry of a block collection belongs to the end of
-- the collection if it is indented at least as deep as the entries, and
-- deeper than the key of the collection. Otherwise it belongs to the node
-- below it, or to the end of an outer collection if no node is below it.
--
-- >>> input = "server:\n  ports:\n  - 80\n  # a\n  # b\n# c\nuser: admin\n"
--
-- >>> T.putStr input
-- server:
--   ports:
--   - 80
--   # a
--   # b
-- # c
-- user: admin
--
-- >>> printComments input
-- root.server (value) after: [Comment "a",Comment "b"]
-- root.user (key) before: [Comment "c"]
--
-- A comment before the closing bracket of a flow collection belongs to the
-- end of the collection.
--
-- >>> input = "ports: [80, 443,\n  # a\n  ]\n"
--
-- >>> T.putStr input
-- ports: [80, 443,
--   # a
--   ]
--
-- >>> printComments input
-- root.ports (value) after: [Comment "a"]

-- $documents
-- The optional @---@ marker starts a document, and the optional @...@ marker
-- ends it. The lines above the directives or the @---@ marker belong to the
-- document. So do the comment on the line of the @...@ marker and the lines
-- below it. Without the markers, the root gets these lines.
--
-- A comment on the line of the @---@ marker belongs to the document, unless
-- the rule for comments at the end of a line gives it to a node.
--
-- >>> input = "# a\n--- # b\n# c\n\nentry: value\n\n# e\n...\n# f\n"
--
-- >>> T.putStr input
-- # a
-- --- # b
-- # c
-- <BLANKLINE>
-- entry: value
-- <BLANKLINE>
-- # e
-- ...
-- # f
--
-- >>> printComments input
-- document before: [Comment "a"]
-- document inline: "b"
-- root before: [Comment "c",EmptyLine]
-- root after: [EmptyLine,Comment "e"]
-- document after: [Comment "f"]
--
-- Between two documents, the first empty line below the @...@ marker ends
-- the lines of the first document. The empty line and the lines below it
-- belong to the second document: to the lines above its @---@ marker, or to
-- its root without the marker.
--
-- >>> input = "x: 1\n...\n# a\n\n# b\n---\ny: 2\n"
--
-- >>> T.putStr input
-- x: 1
-- ...
-- # a
-- <BLANKLINE>
-- # b
-- ---
-- y: 2
--
-- >>> printComments input
-- document after: [Comment "a"]
-- next document
-- document before: [EmptyLine,Comment "b"]
--
-- Without the @...@ marker, the first empty line below the root ends the
-- lines of the root in the same way.
--
-- >>> input = "x: 1\n# a\n\n# b\n---\ny: 2\n"
--
-- >>> T.putStr input
-- x: 1
-- # a
-- <BLANKLINE>
-- # b
-- ---
-- y: 2
--
-- >>> printComments input
-- root after: [Comment "a"]
-- next document
-- document before: [EmptyLine,Comment "b"]
--
-- The lines of a flow collection are between its brackets, so the lines
-- below a flow collection root belong to the document, also without the
-- @...@ marker.
--
-- >>> input = "[80, 443]\n# a\n"
--
-- >>> T.putStr input
-- [80, 443]
-- # a
--
-- >>> printComments input
-- document after: [Comment "a"]
--
-- The renderer writes the markers and the empty lines that these rules
-- need, so that the lines read back at the same places.

-- $emptyLines
-- Empty lines go with the node below them, or with the end of the document.
-- Thus, if a program removes an entry, the gap below the entry stays.
--
-- >>> input = "server:\n  host: localhost\n  # The end of the server.\n\nuser: admin\n"
--
-- >>> T.putStr input
-- server:
--   host: localhost
--   # The end of the server.
-- <BLANKLINE>
-- user: admin
--
-- >>> printComments input
-- root.server (value) after: [Comment "The end of the server."]
-- root.user (key) before: [EmptyLine]
--
-- Empty lines above a comment go with the comment. Several empty lines in a
-- row count as one.
--
-- >>> input = "host: localhost\n\n\n# The port.\nport: 80\n\nuser: admin\n"
--
-- >>> T.putStr input
-- host: localhost
-- <BLANKLINE>
-- <BLANKLINE>
-- # The port.
-- port: 80
-- <BLANKLINE>
-- user: admin
--
-- >>> printComments input
-- root.port (key) before: [EmptyLine,Comment "The port."]
-- root.user (key) before: [EmptyLine]
--
-- One place is an exception. Above the first entry of a block collection,
-- the last empty line stays with the collection. If the lines of a block
-- collection root do not end with an empty line, e.g. lines that a program
-- added, the renderer writes one below them, so that they read back as the
-- lines of the collection.
--
-- >>> input = "# The file.\n\nhost: localhost\n"
--
-- >>> T.putStr input
-- # The file.
-- <BLANKLINE>
-- host: localhost
--
-- >>> printComments input
-- root before: [Comment "The file.",EmptyLine]

-- $setup
-- >>> import Data.Text.IO qualified as T
--
-- >>> :{
-- printComments :: T.Text -> IO ()
-- printComments input = either print docs (parseDocumentsText input)
--   where
--     docs :: [Document] -> IO ()
--     docs = \case
--       d : ds -> doc d >> mapM_ (\d' -> putStrLn "next document" >> doc d') ds
--       [] -> pure ()
--     doc :: Document -> IO ()
--     doc d = do
--       report "document" d.docComments {after = []}
--       node "root" "" d.root
--       report "document" noComments {after = d.docComments.after}
--     node :: String -> String -> Node -> IO ()
--     node path role n = do
--       report (path <> role) n.comments
--       case n.content of
--         Sequence _ items ->
--           sequence_ [node (path <> "[" <> show i <> "]") "" item | (i, item) <- zip [0 :: Int ..] items]
--         Mapping _ entries ->
--           sequence_ [node (path <> "." <> name k) " (key)" k >> node (path <> "." <> name k) " (value)" v | (k, v) <- entries]
--         _ -> pure ()
--     name :: Node -> String
--     name k = case k.content of
--       Scalar _ t -> T.unpack t
--       _ -> "?"
--     report :: String -> Comments -> IO ()
--     report path c =
--       mapM_ putStrLn $
--         [path <> " before: " <> show c.before | not (null c.before)]
--           <> [path <> " inline: " <> show t | Just t <- [c.inline]]
--           <> [path <> " after: " <> show c.after | not (null c.after)]
-- :}
