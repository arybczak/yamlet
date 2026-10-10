module Yamlet.Test.Decode.SyntaxErrors
  ( syntaxErrorTests
  ) where

import Control.Monad
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet
import Yamlet.Syntax qualified as S
import Yamlet.Test.Helpers

syntaxErrorTests :: TestTree
syntaxErrorTests =
  testGroup
    "syntax errors"
    [ testCase "syntax" test_syntaxErrors
    , testCase "directives and tags" test_directiveErrors
    ]

test_syntaxErrors :: Assertion
test_syntaxErrors = do
  let check :: String -> (Int, Int, String) -> T.Text -> Assertion
      check preface expected input =
        assertEqual
          preface
          (Just expected)
          (errorOf (decodeAllText @Value input))
  check
    "bad indentation"
    (3, 2, "unexpected indentation")
    "a:\n  b: 1\n c: 2\n"
  check
    "indicator at the indentation of a key"
    (3, 3, "unexpected '@', a plain scalar cannot start with it, quote the value")
    "dependencies:\n  typescript: ^5.0.0\n  @types/node: ^20.0.0\n"
  check
    "indicator on an indented first line"
    (1, 3, "unexpected '@', a plain scalar cannot start with it, quote the value")
    "  @b: c\n"
  check
    "bracket at the indentation of a list item"
    (3, 3, "unexpected ']'")
    "a:\n  - b\n  ]\n"
  -- The BOM at the start of the input is not content of the first line.
  forM_
    [
      ( "colon in an alias"
      ,
        ( 1
        , 3
        , "the name of the alias includes the ':', write a space before ':' if the alias is a key"
        )
      , "*x: 1"
      )
    ,
      ( "properties on their own line"
      ,
        ( 1
        , 1
        , "an anchor or a tag cannot be on a line of its own here, write it after the key or the '-'"
        )
      , "&a &b"
      )
    , ("tab before a key", (1, 1, "tabs cannot be used for indentation"), "\tx: y")
    ,
      ( "mapping on the start marker line"
      , (1, 6, "unexpected ':', a mapping cannot start on the line of '---'")
      , "--- a: b"
      )
    , ("key among list items", (2, 1, "unexpected key among list items"), "- a\nb: 1")
    , ("list item without a space", (2, 2, "expected a space after '-'"), "- a\n-b")
    ]
    $ \(preface, expected, input) -> do
      check
        preface
        expected
        input
      check
        (preface ++ " after a BOM")
        expected
        ("\xFEFF" <> input)
  check
    "line after a comment below a plain scalar"
    (3, 3, "a comment ends a plain scalar, so this line cannot continue it")
    "a: x\n# c\n  y\n"
  forM_
    [ ("literal scalar", 4, "a: |\n  x\n# c\n  y\n")
    , ("single-quoted scalar", 3, "a: 'x'\n# c\n  y\n")
    , ("flow sequence", 3, "a: [x]\n# c\n  y\n")
    , ("alias", 3, "a: *x\n# c\n  y\n")
    ]
    $ \(node, line, input) ->
      check
        ("line after a comment below a " ++ node)
        (line, 3, "unexpected indentation")
        input
  forM_
    [ ("double-quoted scalar", 2, "a: \"x #y\"\n  b\n")
    , ("single-quoted scalar", 2, "a: 'see #3'\n  b\n")
    , ("quoted scalar in a flow sequence", 2, "a: [x, \"y #z\"]\n  b\n")
    , ("multi-line quoted scalar", 3, "a: \"x\n  y #z\"\n  b\n")
    ]
    $ \(node, line, input) ->
      check
        ("line below a '#' in a " ++ node)
        (line, 3, "unexpected indentation")
        input
  check
    "line after a comment with a quote"
    (2, 3, "a comment ends a plain scalar, so this line cannot continue it")
    "a: x # it's\n  b\n"
  forM_
    [ ("list item", "- # c\nfoo\n")
    , ("list item with an anchor", "- &x # c\nfoo\n")
    , ("list item with a tag", "- !t # c\nfoo\n")
    ]
    $ \(node, input) ->
      check
        ("line after a comment on an empty " ++ node)
        (2, 1, "unexpected key among list items")
        input
  check
    "line after a comment below the header of a block scalar"
    ( 3
    , 3
    , "unexpected indentation, the line has less indentation than the block scalar above it"
    )
    "a: |\n# c\n  y\n"
  check
    "mapping in a plain scalar"
    (1, 11, "unexpected ':', quote the value if it contains \": \"")
    "key: value: other\n"
  check
    "content after a quoted value"
    (1, 14, "unexpected 't' after the end of a quoted scalar")
    "key: \"value\" trailing\n"
  check
    "content after a flow value"
    (1, 12, "unexpected 'i' after the end of a flow collection")
    "x: { y: z }in: valid\n"
  check
    "letter beyond ASCII"
    (1, 4, "unexpected 'é' after the end of a flow collection")
    "[a]é\n"
  check
    "character that cannot be shown"
    (1, 4, "unexpected U+200B after the end of a flow collection")
    "[a]\x200B\n"
  check
    "comment line in a plain scalar"
    (3, 3, "a comment ends a plain scalar, so this line cannot continue it")
    "key: word1\n#  xxx\n  word2\n"
  check
    "comment at the end of a line of a plain scalar"
    (2, 1, "a comment ends a plain scalar, so this line cannot continue it")
    "word1  # comment\nword2\n"
  check
    "directive after a comment"
    ( 3
    , 1
    , "unexpected '%', a directive needs '...' on a line above it to end the document"
    )
    "---\nscalar1 # comment\n%YAML 1.2\n---\nscalar2\n"
  check
    "directive after a mapping"
    ( 2
    , 1
    , "unexpected '%', a directive needs '...' on a line above it to end the document"
    )
    "a: 1\n%YAML 1.2\n---\nb: 2\n"
  check
    "percent sign at the start of a line in a flow sequence"
    (2, 1, "unexpected '%', a plain scalar cannot start with it, quote the value")
    "[a,\n%x]\n"
  check
    "anchor on its own line in a sequence"
    ( 2
    , 1
    , "an anchor or a tag cannot be on a line of its own here, write it after the key or the '-'"
    )
    "- item1\n&node\n- item2\n"
  check
    "tag on its own line after a key"
    ( 2
    , 1
    , "an anchor or a tag cannot be on a line of its own here, write it after the key or the '-'"
    )
    "key: &x\n!!map\n  a: b\n"
  check
    "flow key on two lines"
    (2, 2, "unexpected ':', a key must be on a single line")
    "[23\n]: 42\n"
  check
    "quoted key on two lines"
    (2, 3, "a key must be on a single line")
    "a: 1\n\"c\n d\": 1\n"
  check
    "mapping on the line of the document marker"
    (1, 9, "unexpected ':', a mapping cannot start on the line of '---'")
    "--- key1: value1\n    key2: value2\n"
  check
    "key indented under a value"
    ( 2
    , 4
    , "unexpected ':', this line continues the scalar from the line above, check the indentation and the line above"
    )
    "a: 1\n  b: 2\n"
  check
    "missing closing quote"
    (1, 7, "unterminated double-quoted scalar")
    "name: \"abc\nnext: value\n"
  check
    "badly indented quoted line"
    (2, 1, "invalid indentation of a line in a single-quoted scalar")
    "name: 'abc\nnext'\n"
  check
    "missing colon"
    (2, 4, "expected ':' after the key")
    "a: 1\nb 2\nc: 3\n"
  check
    "missing colon in a list item"
    (2, 8, "expected ':' after the key")
    "- key: value\n  other\n"
  check
    "missing colon before a comment"
    (2, 10, "expected ':' after the key")
    "name: x\nport 8080 # default\n"
  check
    "missing colon after a key before a comment"
    (2, 5, "expected ':' after the key")
    "name: x\nport # default\n"
  check
    "missing colon after a quoted key with a colon"
    (2, 7, "expected ':' after the key")
    "a: 1\n\"b: 2\"\n"
  check
    "missing colon after a key with an escaped quote"
    (2, 10, "expected ':' after the key")
    "a: 1\n\"b\\\"c: d\"\n"
  check
    "unterminated quoted key with a colon"
    (2, 1, "unterminated double-quoted scalar")
    "a: 1\n\"b: 2\n"
  check
    "unterminated single-quoted key"
    (3, 3, "unterminated single-quoted scalar")
    "env:\n  NAME: x\n  'it''s\n"
  check
    "missing space after a colon"
    (2, 3, "expected a space after ':'")
    "a: 1\nb:2\n"
  check
    "line that has its colon"
    (2, 5, "expected an alias name after '*'")
    "a: 1\nb: *\n"
  check
    "anchor without a name"
    (1, 5, "expected an anchor name after '&'")
    "a: & 1\n"
  check
    "alias with an anchor"
    (2, 7, "unexpected '*', an alias cannot have an anchor or a tag")
    "a: &x 1\nb: &y *x\n"
  check
    "alias with a tag in a flow sequence"
    (1, 11, "unexpected '*', an alias cannot have an anchor or a tag")
    "[&x a, !t *x]\n"
  check
    "colon after an alias"
    ( 2
    , 3
    , "the name of the alias includes the ':', write a space before ':' if the alias is a key"
    )
    "a: &x 1\n*x: 2\n"
  check
    "alias without a name in a flow sequence"
    (1, 2, "expected an alias name after '*'")
    "[*, a]\n"
  check
    "tab indentation"
    (2, 1, "tabs cannot be used for indentation")
    "a:\n\tb: 1\n"
  check
    "tab after spaces before a key"
    (2, 3, "tabs cannot be used for indentation")
    "a:\n  \tb: c\n"
  check
    "tab before a scalar continuation"
    (2, 1, "tabs cannot be used for indentation")
    "a: 1\n\t@\n"
  -- A space in place of the tab fails too, but the indentation of the line
  -- above does not.
  check
    "tab before a second key"
    (3, 1, "tabs cannot be used for indentation")
    "a:\n  b: 1\n\tc: 2\n"
  check
    "tab before a second item"
    (3, 1, "tabs cannot be used for indentation")
    "a:\n  - 1\n\t- 2\n"
  check
    "tab below a comment"
    (5, 1, "tabs cannot be used for indentation")
    "a:\n  b: 1\n  # c\n\n\tc: 2\n"
  check
    "tab on a blank line of a block scalar"
    (3, 1, "tabs cannot be used for indentation")
    "a: |\n  x\n\t\n  y\n"
  check
    "tab before a comment below a block scalar"
    (3, 1, "tabs cannot be used for indentation")
    "a: |\n  x\n\t# c\nb: 1\n"
  check
    "tab on a blank line of a plain scalar"
    (2, 1, "tabs cannot be used for indentation")
    "a: b\n\t\n c\n"
  check
    "tab on a blank line above a line indented too much"
    (3, 3, "unexpected indentation")
    "a: 1\n\t\n  b: 2\n"
  check
    "tab after a list item indicator"
    (3, 3, "tabs cannot be used for indentation")
    "x:\n- a\n- \tb: c\n"
  check
    "tab right after a list item indicator"
    (1, 2, "tabs cannot be used for indentation")
    "-\tname: x\n"
  check
    "tab after an explicit key indicator"
    (2, 3, "tabs cannot be used for indentation")
    "x: 1\n? \ta: b\n"
  check
    "tab after a value indicator"
    (2, 2, "tabs cannot be used for indentation")
    "? a\n:\tb: c\n"
  -- With a space in place of the tab, the parser fails at the same place.
  check
    "tab before a list after a key"
    (1, 6, "unexpected '-', a list cannot start on the line of its key")
    "key:\t- a\n"
  -- With spaces in place of the tab, the parser fails at the same place.
  check
    "tab before an indicator"
    (1, 2, "unexpected '@', a plain scalar cannot start with it, quote the value")
    "\t@\n"
  check
    "tab before a bracket"
    (1, 2, "unexpected ']'")
    "\t]\n"
  check
    "tab after an end marker"
    (3, 2, "unexpected '@', a plain scalar cannot start with it, quote the value")
    "a\n...\n\t@\n"
  check
    "content after spaces after an end marker"
    (2, 7, "unexpected content after the document end marker (...)")
    "--- a\n...   x\n"
  check
    "unterminated string"
    (1, 6, "unterminated double-quoted scalar")
    "key: \"abc\n"
  check
    "flow sequence before a key"
    (1, 6, "unterminated flow sequence")
    "key: [a, b\nc: d\n"
  check
    "flow sequence at the end"
    (1, 6, "unterminated flow sequence")
    "key: [a, b\n"
  check
    "flow sequence before a comment"
    (1, 6, "unterminated flow sequence")
    "key: [a, b # c\nd: e\n"
  check
    "flow sequence before a key with a flow sequence"
    (1, 6, "unterminated flow sequence")
    "key: [a, b\nc: [d]\n"
  check
    "flow sequence on a line indented too little"
    (2, 1, "the line is indented too little to continue the flow sequence")
    "a: [b,\nc]\n"
  check
    "flow mapping on lines indented too little"
    (2, 1, "the line is indented too little to continue the flow mapping")
    "a: {x: 1,\ny: 2,\n  z: 3}\n"
  check
    "closing bracket indented too little"
    (4, 1, "']' is indented too little to end the flow sequence")
    "key: [\n  a,\n  b\n]\n"
  check
    "closing brace after a comment line"
    (3, 1, "'}' is indented too little to end the flow mapping")
    "key: {\n  # c\n}\n"
  check
    "tab in a flow sequence"
    (2, 1, "tabs cannot be used for indentation")
    "a: [\n\tb\n]\n"
  check
    "tab in a double-quoted scalar"
    (2, 1, "tabs cannot be used for indentation")
    "a: \"x\n\ty\"\n"
  check
    "tab after a blank line in a single-quoted scalar"
    (3, 1, "tabs cannot be used for indentation")
    "a: 'x\n\n\ty'\n"
  check
    "tab on a blank line of a double-quoted scalar"
    (2, 1, "tabs cannot be used for indentation")
    "a: \"x\n\t\n y\"\n"
  check
    "block scalar in a flow sequence"
    (1, 2, "unexpected '|', a block scalar cannot be inside a flow collection")
    "[|\n  x\n]\n"
  check
    "block scalar indicator after a quoted scalar"
    (1, 8, "unexpected '|' after the end of a quoted scalar")
    "a: 'x' |\n"
  check
    "block scalar indicator after a flow collection"
    (1, 8, "unexpected '|' after the end of a flow collection")
    "a: [x] |\n"
  check
    "block scalar indicator at the start of a line"
    (2, 1, "unexpected '|'")
    "a: b\n| x\n"
  check
    "dash in a flow sequence"
    ( 1
    , 2
    , "unexpected '-', a list item cannot be inside a flow collection, quote '-' if it is a string"
    )
    "[-]\n"
  check
    "empty flow entry"
    (1, 4, "unexpected ',', a flow collection cannot have an empty entry")
    "[1,,2]\n"
  check
    "content after a flow sequence"
    (1, 14, "expected ',' or ']'")
    "key: [a, \"b\" c]\n"
  check
    "flow mapping at the end"
    (1, 1, "unterminated flow mapping")
    "{\"a\": 1,\n \"b\": 2\n"
  check
    "flow mapping before a marker"
    (1, 1, "unterminated flow mapping")
    "{a: 1\n---\nb\n"
  check
    "start marker in a double-quoted scalar"
    (2, 1, "unexpected '---' in a double-quoted scalar, indent the line")
    "a: \"x\n---\n  y\"\n"
  check
    "end marker in a single-quoted scalar"
    (2, 1, "unexpected '...' in a single-quoted scalar, indent the line")
    "a: 'x\n...\n  y'\n"
  check
    "start marker in a flow sequence"
    (2, 1, "unexpected '---' in a flow sequence, indent the line")
    "a: [x,\n---\n  y]\n"
  check
    "end marker in a flow mapping"
    (2, 1, "unexpected '...' in a flow mapping, indent the line")
    "a: {x: 1,\n...\n  y: 2}\n"
  check
    "flow sequence before a document with its own"
    (1, 7, "unterminated flow sequence")
    "args: [a, b\n---\nargs: [c]\n"
  check
    "flow mapping before a document with its own"
    (1, 7, "unterminated flow mapping")
    "args: {a: b\n---\nx: {c: d}\n"
  check
    "double-quoted scalar before a document with its own"
    (1, 7, "unterminated double-quoted scalar")
    "name: \"web\n---\nname: \"db\"\n"
  check
    "single-quoted scalar before a document with its own"
    (1, 7, "unterminated single-quoted scalar")
    "name: 'web\n---\nname: 'it''s'\n"
  check
    "single-quoted scalar before a document with a quote in a plain scalar"
    (1, 4, "unterminated single-quoted scalar")
    "a: 'oops\n---\nb: it's here\n"
  check
    "escaped quote after a marker"
    (2, 1, "unexpected '---' in a double-quoted scalar, indent the line")
    "a: \"x\n---\n  y \\\"z\"\n"
  check
    "start marker after a missing quote"
    (1, 4, "unterminated double-quoted scalar")
    "a: \"x\n---\nb: c\n"
  check
    "missing colon in a flow mapping"
    (1, 6, "expected ':', ',' or '}'")
    "{\"a\" 1}"
  check
    "missing comma after a value"
    (1, 12, "expected ',' or '}'")
    "{\"a\": 1 \"b\": 2}"
  check
    "quote in a single-quoted scalar"
    (1, 10, "unexpected 's' after a single-quoted scalar, write '' for a quote inside it")
    "msg: 'it's here'\n"
  check
    "quote in a tag"
    (1, 6, "unexpected '!'")
    "&b !'!str ' x '\n"
  check
    "quote in a double-quoted scalar"
    ( 1
    , 12
    , "unexpected 'h' after a double-quoted scalar, write \\\" for a quote inside it"
    )
    "msg: \"say \"hi\"\"\n"
  check
    "quote in a quoted scalar in a flow sequence"
    ( 1
    , 5
    , "unexpected 'b' after a double-quoted scalar, write \\\" for a quote inside it"
    )
    "[\"a\"b]\n"
  check
    "comment after a quote"
    (1, 7, "unexpected '#', a comment needs a space before it")
    "a: \"x\"#c\n"
  check
    "comment after a flow sequence"
    (1, 7, "unexpected '#', a comment needs a space before it")
    "a: [1]#c\n"
  check
    "reserved indicator"
    (1, 7, "unexpected '@', a plain scalar cannot start with it, quote the value")
    "user: @admin\n"
  check
    "reserved indicator in a flow sequence"
    (1, 5, "unexpected '`', a plain scalar cannot start with it, quote the value")
    "[a, `b`]\n"
  check
    "key among indented list items after a comment"
    (5, 3, "unexpected key among list items")
    "a:\n  - x\n\n  # c\n  b: 1\n"
  check
    "list item among keys"
    (3, 3, "unexpected list item among mapping entries")
    "a:\n  b: 1\n  - x\n"
  check
    "list item among top keys"
    (2, 1, "unexpected list item among mapping entries")
    "a: 1\n- b\n"
  check
    "brace after a list item"
    (2, 1, "unexpected '}'")
    "- a\n}\n"
  check
    "text after a block scalar header"
    (1, 6, "the content of a block scalar starts on the next line")
    "s: | text\n"
  check
    "zero indentation indicator"
    (1, 5, "the indentation indicator of a block scalar must be from 1 to 9")
    "s: |0\n  x\n"
  check
    "long key"
    (1, 1103, "a key can be at most 1024 characters long, write a longer key after '? '")
    ("\"" <> T.replicate 1100 "k" <> "\": 1\n")
  check
    "long flow mapping as a key"
    (1, 1106, "a key can be at most 1024 characters long, write a longer key after '? '")
    ("{a: " <> T.replicate 1100 "k" <> "}: 1\n")
  check
    "spaces after a key count toward its length"
    (1, 1026, "a key can be at most 1024 characters long, write a longer key after '? '")
    ("a" <> T.replicate 1024 " " <> ": 1\n")
  check
    "spaces after a key in a flow sequence"
    (1, 1029, "a key can be at most 1024 characters long, write a longer key after '? '")
    ("['a'" <> T.replicate 1024 " " <> ": 1]\n")
  check
    "long key in a flow sequence"
    (1, 1030, "a key can be at most 1024 characters long, write a longer key after '? '")
    ("[a, " <> T.replicate 1025 "k" <> ": v]\n")
  check
    "list on the line of its anchor"
    (1, 9, "unexpected '-', a list cannot start on the line of its anchor or tag")
    "&anchor - sequence entry\n"
  check
    "list on the line of its key"
    (1, 4, "unexpected '-', a list cannot start on the line of its key")
    "a: - b\n"
  check
    "list on the line of a start marker"
    (1, 5, "unexpected '-', a list cannot start on the line of '---'")
    "--- - a\n"
  check
    "line of a block scalar"
    ( 3
    , 3
    , "unexpected indentation, the line has less indentation than the block scalar above it"
    )
    "s: >- # folded\n    line1\n  line2\n"
  check
    "invalid escape"
    (1, 8, "invalid escape sequence, write \\\\ for a backslash or use single quotes")
    "key: \"a\\qb\"\n"
  check
    "Windows path"
    (1, 10, "invalid escape sequence, write \\\\ for a backslash or use single quotes")
    "path: \"C:\\Users\\me\"\n"
  check
    "undefined alias"
    (2, 4, "undefined alias *x")
    "a: 1\nb: *x\n"
  check
    "invalid character"
    (1, 4, "invalid character U+0001")
    "a: \x01\n"
  check
    "backslash at the end of the input"
    (1, 4, "unterminated double-quoted scalar")
    "a: \"b\\"
  check
    "backslash at the end of a key"
    (1, 2, "unterminated double-quoted scalar")
    "[\"a\\"
  check
    "noncharacter U+FFFE"
    (1, 4, "invalid character U+FFFE")
    "a: \xFFFE\n"
  check
    "noncharacter U+FFFF"
    (1, 5, "invalid character U+FFFF")
    "a: b\xFFFF\n"
  check
    "delete in a plain scalar"
    (1, 5, "invalid character U+007F")
    "a: x\DELy\n"
  check
    "delete at the start of a line"
    (1, 1, "invalid character U+007F")
    "\DEL\n"
  check
    "C1 control character in a comment"
    (1, 9, "invalid character U+0080")
    "a: b # c\x80\n"
  check
    "C1 control character in a tag"
    (1, 6, "invalid character U+0080")
    "a: !x\x80 1\n"
  check
    "noncharacter in a block scalar"
    (2, 4, "invalid character U+FFFF")
    "a: |\n  x\xFFFF\n"
  check
    "C1 control character after a quoted one"
    (2, 4, "invalid character U+0080")
    "- \"\x80\"\n- b\x80\n"
  check
    "byte order mark before a C1 control character"
    (1, 5, "unexpected byte order mark")
    "a: x\xFEFF\x80\n"

test_directiveErrors :: Assertion
test_directiveErrors = do
  let check :: String -> (Int, Int, String) -> T.Text -> Assertion
      check preface expected input =
        assertEqual
          preface
          (Just expected)
          (errorOf (decodeAllText @Value input))
  check
    "undefined tag handle"
    (1, 1, "undefined tag handle !e!")
    "!e!foo bar\n"
  check
    "unsupported version"
    (1, 1, "unsupported YAML version 2.0")
    "%YAML 2.0\n--- a\n"
  check
    "version without a minor number"
    (1, 7, "expected a version such as 1.2 after %YAML")
    "%YAML 1\n--- a\n"
  check
    "content after the version"
    (1, 11, "unexpected content after the %YAML version")
    "%YAML 1.2 x\n--- a\n"
  check
    "tag directive without a prefix"
    (1, 9, "expected a prefix after the tag handle, e.g. tag:example.com,2000:")
    "%TAG !e!\n--- a\n"
  check
    "invalid tag handle"
    (1, 6, "invalid tag handle")
    "%TAG e tag:x,2000:\n--- a\n"
  check
    "tag handle without its closing !"
    (1, 6, "invalid tag handle")
    "%TAG !e tag:x,2000:\n--- a\n"
  check
    "tag handle with an invalid character"
    (1, 6, "invalid tag handle")
    "%TAG !e_x! tag:x,2000:\n--- a\n"
  check
    "tag handle before the prefix without a space"
    (1, 9, "expected a space after the tag handle")
    "%TAG !e!tag:x,2000:\n--- a\n"
  check
    "version beyond the limit"
    (1, 1, "unsupported YAML version")
    "%YAML 1000001.2\n--- a\n"
  check
    "minor version beyond the limit"
    (1, 1, "unsupported YAML version")
    "%YAML 1.1000001\n--- a\n"
  check
    "version beyond Int"
    (1, 1, "unsupported YAML version")
    "%YAML 18446744073709551617.2\n--- a\n"
  check
    "minor version beyond Int"
    (1, 1, "unsupported YAML version")
    ("%YAML 1." <> T.replicate 100000 "9" <> "\n--- a\n")
  assertEqual
    "minor version at the limit"
    (Right [Just (S.YamlVersion 1 1000000)])
    (map (.version) <$> S.parseDocumentsText "%YAML 1.1000000\n--- a\n")
  assertEqual
    "version with leading zeros"
    (Right [Just (S.YamlVersion 1 2)])
    (map (.version) <$> S.parseDocumentsText "%YAML 001.0002\n--- a\n")
  check
    "verbatim tag without a name"
    (1, 1, "invalid verbatim tag")
    "!<!> a\n"
  check
    "verbatim tag without a scheme"
    (1, 1, "invalid verbatim tag")
    "!<$:?> a\n"
  check
    "empty verbatim tag"
    (1, 1, "invalid verbatim tag")
    "!<> a\n"
  let badEscape = "invalid escape in the tag, write '%' and two hexadecimal digits"
  check
    "escape without digits in a tag"
    (1, 6, badEscape)
    "x: !a%zz b\n"
  check
    "escape with one digit in a tag"
    (1, 6, badEscape)
    "x: !a%4 b\n"
  check
    "escape without digits in a tag prefix"
    (1, 8, badEscape)
    "%TAG ! %\xE9\n--- a\n"
  check
    "secondary handle without a suffix"
    (1, 6, "expected the rest of the tag after !!")
    "a: !! z\n"
  check
    "invalid UTF-8 in a tag"
    (1, 1, "the escapes of the tag are not valid UTF-8")
    "!!str%FF a\n"
  assertEqual
    "character from the escapes of the prefix and the suffix"
    (Right ["tag:\xE9"])
    $ map (\d -> case d.root.props.tag of S.Tag t -> t; _ -> "")
      <$> S.parseDocumentsText "%TAG !e! tag:%C3\n--- !e!%A9 a\n"
  assertEqual
    "valid verbatim tags"
    (Right ["!bar", "tag:yaml.org,2002:str"])
    (map valueTag <$> decodeText @[Value] "[!<!bar> a, !<tag:yaml.org,2002:str> b]")
  assertEqual
    "escapes of verbatim tags"
    (Right ["!foo!", "tag:example.com,2000:\xE9"])
    $ map valueTag
      <$> decodeText @[Value] "[!<!foo%21> a, !<tag:example.com,2000:%C3%A9> b]"
  check
    "invalid UTF-8 in a verbatim tag"
    (1, 1, "the escapes of the tag are not valid UTF-8")
    "!<!a%FF> b\n"
