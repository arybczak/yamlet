-- | Decoding into the types of the library and of the user with the
-- functions of the parser, and the decoded values: their locations, and that
-- they hold no thunks and no slices of the input.
module Yamlet.Test.Decode.Values
  ( valueTests
  ) where

import Control.Exception
import Data.Bifunctor
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Internal qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet
import Yamlet.Internal.Parser.Monad qualified as P
import Yamlet.Syntax qualified as S
import Yamlet.Test.Decode.Helpers
import Yamlet.Test.Helpers
import Yamlet.Test.Helpers.Thunks

valueTests :: TestTree
valueTests =
  testGroup
    "values"
    [ testCase "record" test_record
    , testCase "notFollowedBy" test_notFollowedBy
    , testCase "containers" test_containers
    , testCase "copies" test_copies
    , testCase "JSON" test_json
    , testCase "aliases" test_aliases
    , testCase "optional keys" test_optionalKeys
    , testCase "located values" test_located
    , testCase "syntax tree" test_syntaxTree
    , testCase "no thunks" test_noThunks
    ]

-- | The decoders of the types that the library defines return values without
-- thunks.
test_noThunks :: Assertion
test_noThunks = do
  check "value" (decodeText @Value input)
  check "node" (decodeText @S.Node input)
  check "commented values" (decodeText @(M.Map Value (Commented Value)) input)
  check "located values" (decodeText @(M.Map Value (Located Value)) input)
  check "value with its document" (decodeWithDocument @Value input)
  where
    input :: T.Text
    input =
      T.unlines
        [ "# The anchor."
        , "a: &x [1, 2.5, -.inf, \"s\"] # the list"
        , "b: *x"
        , "? [k, 1]"
        , ": {n: null, t: true, !custom tag: !custom v}"
        , "c: |"
        , "  text"
        ]

    check :: String -> Either (NE.NonEmpty Error) a -> Assertion
    check preface = \case
      Right x -> thunks x >>= assertEqual preface []
      Left errs -> assertFailure (preface ++ ": " ++ show errs)

-- | An error inside 'P.notFollowedBy' is not lost.
test_notFollowedBy :: Assertion
test_notFollowedBy = do
  let T.Text arr off len = "a"
      e =
        P.Env
          { P.array = arr
          , P.base = off
          , P.end = off + len
          , P.streamEnd = off + len
          , P.handles = M.empty
          }
  case P.runParser e off (P.notFollowedBy (P.throwAt off "boom")) of
    Left (P.ParseError _ msg) ->
      assertEqual
        "message"
        "boom"
        msg
    Left (P.UnexpectedParseError _ _) -> assertFailure "expected an error with a message"
    Right _ -> assertFailure "expected an error"

test_containers :: Assertion
test_containers = do
  assertEqual
    "set"
    (Right (Set.fromList [1, 2, 3]))
    (decodeText @(Set.Set Int) "[3, 1, 2]")
  assertEqual
    "set with a duplicate"
    (Just ((1, 5, "duplicate element 1.0"), (1, 2, "the first element 1")))
    (errorWithNote (decodeText @(Set.Set Double) "[1, 1.0]"))
  assertEqual
    "set with an equal element"
    (Just ((1, 5, "duplicate element 1"), (1, 2, "the first element 1")))
    (errorWithNote (decodeText @(Set.Set Int) "[1, 1]"))
  assertEqual
    "int map"
    (Right (IM.fromList [(1, "a"), (2, "b")]))
    (decodeText @(IM.IntMap T.Text) "{2: b, 1: a}")
  assertEqual
    "int map with a duplicate key"
    ( Just
        ( (1, 8, "duplicate key 0x1, the same value as the first key")
        , (1, 2, "the first key 1")
        )
    )
    (errorWithNote (decodeText @(IM.IntMap T.Text) "{1: a, 0x1: b}"))
  assertEqual
    "int set"
    (Right (IS.fromList [1, 2, 3]))
    (decodeText @IS.IntSet "[3, 1, 2]")
  assertEqual
    "int set with a duplicate"
    (Just ((1, 5, "duplicate element 0x1"), (1, 2, "the first element 1")))
    (errorWithNote (decodeText @IS.IntSet "[1, 0x1]"))
  assertEqual
    "sequence"
    (Right (Seq.fromList [1, 2]))
    (decodeText @(Seq.Seq Int) "[1, 2]")
  assertEqual
    "left"
    (Right (Left 1))
    (decodeText @(Either Int T.Text) "{Left: 1}")
  assertEqual
    "right"
    (Right (Right "a"))
    (decodeText @(Either Int T.Text) "{Right: a}")
  assertEqual
    "either with another key"
    (Just (1, 2, "expected the key Left or Right"))
    (errorOf (decodeText @(Either Int Int) "{Up: 1}"))
  assertEqual
    "either with two keys"
    (Just (1, 1, "expected a mapping with one key, Left or Right"))
    (errorOf (decodeText @(Either Int Int) "{Left: 1, Right: 2}"))
  assertEqual
    "tuple of 4"
    (Right (1, 'a', True, "b"))
    (decodeText @(Int, Char, Bool, T.Text) "[1, a, true, b]")
  assertEqual
    "tuple of 10"
    (Right (1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    $ decodeText @(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
      "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
  assertEqual
    "tuple of 10 with the wrong size"
    (Just (1, 1, "expected a list of 10 elements, but got 1"))
    (errorOf (decodeText @(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) "[1]"))

test_record :: Assertion
test_record = do
  assertEqual
    "full"
    (Right Config {name = "x", paths = ["a", "b"], jobs = 4})
    (decodeText "name: x\npaths: [a, b]\njobs: 4\n")
  assertEqual
    "defaults"
    (Right Config {name = "x", paths = [], jobs = 1})
    (decodeText "name: x\npaths:\n")
  assertEqual
    "keys of a map that convert to the same key"
    (Just ((2, 1, "duplicate key 1.0 after conversion"), (1, 1, "the first key 1")))
    (errorWithNote (decodeText @(M.Map Double Int) "1: 1\n1.0: 2\n"))
  assertEqual
    "string keys with the same text"
    (Just ((2, 6, "duplicate key \"name\""), (1, 1, "the first key \"name\"")))
    (errorWithNote (decodeText @Config "name: x\n!foo name: y\n"))
  assertEqual
    "duplicate keys that are not ASCII"
    (Just ((2, 1, "duplicate key \"ż\""), (1, 1, "the first key \"ż\"")))
    (errorWithNote (decodeText @Value "ż: 1\nż: 2\n"))
  assertEqual
    "several string keys with the same text and a bad field"
    [ (2, 1, "duplicate key \"name\"")
    , (1, 6, "the first key \"name\"")
    , (3, 7, "expected an integer, but got a string")
    , (4, 6, "duplicate key \"jobs\"")
    , (3, 1, "the first key \"jobs\"")
    ]
    (errorsOf (decodeText @Config "!foo name: x\nname: y\njobs: z\n!foo jobs: 4\n"))

-- | Decoded texts and error lines do not point into the input.
test_copies :: Assertion
test_copies = do
  case decodeText @(M.Map T.Text T.Text) "key: value\nother: text\n" of
    Left err -> assertFailure (show err)
    Right m -> assertBool "texts are copies" $ all isCopy (M.keys m ++ M.elems m)
  case decodeText @Int "a: 1\nb: [\n" of
    Left errs ->
      assertBool "the source line is a copy" $ all (isCopy . (.sourceLine)) errs
    Right _ -> assertFailure "expected an error"
  case S.parseDocumentsText "key: &a value\nother: *a\n" of
    Left err -> assertFailure (show err)
    Right docs ->
      assertBool "syntax texts are copies" $
        all (all isCopy . texts . (.root) . S.copyDocument) docs
  case decodeText @(M.Map T.Text Node) "key: value\nother: [a, &x b] # c\n" of
    Left err -> assertFailure (show err)
    Right m ->
      assertBool "texts of kept nodes are copies" $ all (all isCopy . texts) (M.elems m)
  case decodeText @Value "a: !x [b, !y c]\n" of
    Left err -> assertFailure (show err)
    Right v -> assertBool "texts of values are copies" $ all isCopy (valueTexts v)
  -- A lazy copy would keep the input alive until the program forces it.
  case decodeText @[T.Text] "- a\n- b\n" of
    Left err -> assertFailure (show err)
    Right xs -> do
      _ <- evaluate (length xs)
      mapM thunks xs >>= assertEqual "items of a list are copies, not thunks" [] . concat
  case S.parseDocumentsText "a: 1\nb: 2\n" of
    Right [doc]
      | Right keys <- runParser (withMapping (pure . objectKeys)) doc.root -> do
          _ <- evaluate (length keys)
          mapM thunks keys
            >>= assertEqual "keys of an object are copies, not thunks" [] . concat
    _ -> assertFailure "expected the keys of the mapping"
  where
    -- A copy starts at the beginning of its own array.
    isCopy :: T.Text -> Bool
    isCopy (T.Text _ off _) = off == 0

    texts :: S.Node -> [T.Text]
    texts n = case n.content of
      S.ScalarContent _ t -> t : maybe [] pure n.props.anchor
      S.SequenceContent _ xs -> concatMap texts xs
      S.MappingContent _ kvs -> concatMap (\(k, v) -> texts k ++ texts v) kvs
      S.AliasContent name -> [name]

    valueTexts :: Value -> [T.Text]
    valueTexts = \case
      String t -> [t]
      Sequence xs -> concatMap valueTexts xs
      Mapping kvs -> concatMap (\(k, v) -> valueTexts k ++ valueTexts v) kvs
      Tagged tag v -> tag : valueTexts v
      _ -> []

-- | JSON is valid YAML, including the escapes that JSON encoders write.
test_json :: Assertion
test_json = do
  assertEqual
    "document"
    (Right (M.fromList [("a", [1.5, -2e3]), ("b\tc", [])]))
    (decodeText @(M.Map T.Text [Double]) "{\"a\":[1.5,-2E3],\n\t\"b\\tc\": []}")
  assertEqual
    "characters beyond C0 that only quoted scalars can contain"
    (Right (M.fromList [("k\x9F", ["x\DEL", "\x80", "\xFFFE\xFFFF", "'\DEL'"])]))
    $ decodeText @(M.Map T.Text [T.Text])
      "{\"k\x9F\": [\"x\DEL\", \"\x80\", \"\xFFFE\xFFFF\", '''\DEL''']}"
  assertEqual
    "surrogate pair"
    (Right ["\x1F600", "a\x10000z"])
    (decodeText @[T.Text] "[\"\\ud83d\\ude00\", \"a\\uD800\\uDC00z\"]")
  assertEqual
    "lone high surrogate"
    (Just (1, 3, "invalid escape sequence"))
    (errorOf (decodeText @[T.Text] "[\"\\ud83d\"]"))
  assertEqual
    "high surrogate without a low one"
    (Just (1, 3, "invalid escape sequence"))
    (errorOf (decodeText @[T.Text] "[\"\\ud83d\\u0041\"]"))
  assertEqual
    "lone low surrogate"
    (Just (1, 3, "invalid escape sequence"))
    (errorOf (decodeText @[T.Text] "[\"\\ude00\"]"))

test_aliases :: Assertion
test_aliases = do
  assertEqual
    "map"
    (Right (M.fromList [("a", [1, 2]), ("b", [1, 2])]))
    (decodeText @(M.Map T.Text [Int]) "a: &x [1, 2]\nb: *x\n")
  assertEqual
    "anchor before a string with a less-than sign"
    (Right (M.fromList [("a", "<x"), ("b", "<x")]))
    (decodeText @(M.Map T.Text T.Text) "a: &x \"<x\"\nb: *x\n")
  assertEqual
    "anchor inside a node with the same anchor"
    (Right (Sequence [Sequence [Int 1], Int 1]))
    (decodeText @Value "- &a [&a 1]\n- *a\n")
  assertEqual
    "anchor inside a node with the same anchor, typed"
    (Right ([1], 1))
    (decodeText @([Int], Int) "- &a [&a 1]\n- *a\n")
  assertEqual
    "anchor inside a mapping with the same anchor"
    (Right (M.fromList [("x", 1)], 1))
    (decodeText @(M.Map T.Text Int, Int) "- &a {x: &a 1}\n- *a\n")
  assertEqual
    "error inside an alias"
    [ (1, 11, "expected an integer, but got a string")
    , (2, 4, "expected an integer, but got a string")
    , (3, 4, "expected an integer, but got a string")
    ]
    (errorsOf (decodeText @(M.Map T.Text [Int]) "a: &x [1, x]\nb: *x\nc: *x\n"))
  assertEqual
    "path of an error inside an alias"
    (Left [(2, 3, [Index 1])])
    $ first
      ( map (\err -> (err.location.line, err.location.column, pathElements err.path))
          . NE.toList
      )
      (decodeText @([T.Text], [Int]) "- &x [a, b]\n- *x\n")

test_optionalKeys :: Assertion
test_optionalKeys = do
  let check :: String -> (Maybe (Maybe Int), Maybe (Maybe Int)) -> T.Text -> Assertion
      check preface expected input =
        assertEqual
          preface
          (Right (Right expected))
          $ runParser
            ( withMapping $ \o ->
                (,)
                  <$> parseFieldMaybe o "a"
                  <*> parseFieldIfPresent o "a"
            )
            <$> decodeText input
  check
    "missing"
    (Nothing, Nothing)
    "b: 1\n"
  check
    "null"
    (Nothing, Just Nothing)
    "a: null\n"
  check
    "value"
    (Just (Just 1), Just (Just 1))
    "a: 1\n"
  let explicit
        :: String
        -> Either (NE.NonEmpty (Offset, String)) (Int, Maybe Int, Maybe (Maybe Int))
        -> T.Text
        -> Assertion
      explicit preface expected input =
        assertEqual
          preface
          (Right expected)
          $ runParser
            ( withMapping $ \o ->
                (,,)
                  <$> parseFieldWith small o "a"
                  <*> parseFieldMaybeWith small o "b"
                  <*> parseFieldIfPresentWith (parseYaml @(Maybe Int)) o "b"
            )
            <$> decodeText input
      small :: Node -> Parser Int
      small = withInt $ \i -> if i < 10 then pure (fromInteger i) else fail "too large"
  explicit
    "explicit, missing"
    (Right (1, Nothing, Nothing))
    "a: 1\n"
  explicit
    "explicit, null"
    (Right (1, Nothing, Just Nothing))
    "a: 1\nb: null\n"
  explicit
    "explicit, value"
    (Right (1, Just 2, Just (Just 2)))
    "a: 1\nb: 2\n"
  explicit
    "explicit, missing key"
    (Left (pure (Offset 0, "missing key \"a\"")))
    "b: 1\n"
  explicit
    "explicit, bad value"
    (Left (pure (Offset 3, "too large")))
    "a: 20\n"
  let keyError
        :: (Object -> T.Text -> Parser (Maybe Int))
        -> Either (NE.NonEmpty (Offset, String)) (Maybe Int)
      keyError op =
        either
          (error . show)
          (runParser (withMapping (`op` "404")))
          (decodeText "200: 1\n404: 2\n")
      integerKey :: Either (NE.NonEmpty (Offset, String)) (Maybe Int)
      integerKey = Left (pure (Offset 7, "the key 404 is an integer, not a string"))
  assertEqual
    "optional integer key"
    integerKey
    (keyError parseFieldMaybe)
  assertEqual
    "optional integer key, null as a value"
    integerKey
    (keyError parseFieldIfPresent)
  assertEqual
    "explicit optional integer key"
    integerKey
    (keyError (parseFieldMaybeWith parseYaml))
  assertEqual
    "explicit optional integer key, null as a value"
    integerKey
    (keyError (parseFieldIfPresentWith parseYaml))

-- | A located value keeps the offset of its node, and the errors at its offset
-- have lines, columns and paths.
test_located :: Assertion
test_located = do
  assertEqual
    "items"
    (Right [Located "a" (Offset 1), Located "b" (Offset 4)])
    (decodeText @[Located T.Text] "[a, b]")
  let input = "skip:\n  - x\n  - y\n"
  case decodeWithDocument @(M.Map T.Text [Located T.Text]) input of
    Right (m, doc) -> do
      let errs =
            [ (item.offset, "unknown package " ++ show item.value)
            | item <- M.findWithDefault [] "skip" m
            , item.value == "y"
            ]
      assertEqual
        "error at a located value"
        [(3, 5, "skip[1]", "unknown package \"y\"")]
        [ (e.location.line, e.location.column, renderPath e.path, e.message)
        | e <- documentErrors input doc errs
        ]
      assertEqual
        "error without an offset"
        ["conf.yml: not from the input"]
        $ map
          (prettyError "conf.yml")
          (documentErrors input doc [(noOffset, "not from the input")])
    Left errs -> assertFailure (show errs)
  assertEqual
    "second document"
    (errorOf (decodeText @Int "1\n--- 2\n"))
    (errorOf (decodeWithDocument @Int "1\n--- 2\n"))
  assertEqual
    "empty stream"
    ( Right
        ( Nothing
        , S.document $
            S.Node
              { S.offset = Offset 0
              , S.endOffset = Offset 0
              , S.props = S.noProps
              , S.comments = S.noComments
              , S.content = S.ScalarContent S.Plain ""
              }
        )
    )
    (decodeWithDocument @(Maybe Int) "")
  assertEqual
    "comments of the key"
    (Right (Just (Offset 3, Just "c")))
    $ fmap (\l -> (l.offset, l.value.comments.inline)) . M.lookup "a"
      <$> decodeText @(M.Map T.Text (Located (Commented T.Text))) "a: x # c\n"
  assertEqual
    "encoded"
    "a: 1\n"
    (encodeText @(M.Map T.Text (Located Int)) (M.fromList [("a", Located 1 (Offset 7))]))

test_syntaxTree :: Assertion
test_syntaxTree = do
  let input = "# The build.\nname: x\njobs: 4 # At most.\n"
  case S.parseDocumentsText input of
    Right [doc] -> do
      assertEqual
        "parsed"
        (Right Config {name = "x", paths = [], jobs = 4})
        (decodeDocument input doc)
      let changed = doc {S.root = S.mappingNode [(S.plainNode "name", S.plainNode "y")]}
      assertEqual
        "changed"
        (Right Config {name = "y", paths = [], jobs = 1})
        (decodeDocument input changed)
    r -> assertFailure (show r)
  case S.parseDocumentsText "name: x\njobs: many\n" of
    Right [doc] -> do
      assertEqual
        "type error"
        (Just (2, 7, "expected an integer, but got a string"))
        (errorOf (decodeDocument @Config "name: x\njobs: many\n" doc))
      let firstLines :: Either (NE.NonEmpty Error) Config -> [String]
          firstLines =
            either (map (takeWhile (/= '\n') . prettyError "f.yaml") . NE.toList) (const [])
      case doc.root.content of
        S.MappingContent _ [_, (_, jobs)] -> do
          let mixed =
                S.document
                  (S.mappingNode [(S.plainNode "name", S.plainNode "y"), (S.plainNode "jobs", jobs)])
          assertEqual
            "parsed node in a built document, with its input"
            ["f.yaml:2:7: jobs: expected an integer, but got a string"]
            (firstLines (decodeDocument "name: x\njobs: many\n" mixed))
          assertEqual
            "parsed node in a built document, without its input"
            ["f.yaml: jobs: expected an integer, but got a string"]
            (firstLines (decodeDocument "" mixed))
        c -> assertFailure (show c)
    r -> assertFailure (show r)
  let key = S.plainNode "a"
      built = S.document (S.mappingNode [(key, key), (key, key)])
  assertEqual
    "built"
    (Just ((0, 0, "duplicate key \"a\""), (0, 0, "the first key \"a\"")))
    (errorWithNote (decodeDocument @Value "" built))
  assertEqual
    "built, rendered"
    (Left ["built.yaml: duplicate key \"a\"", "built.yaml: the first key \"a\""])
    $ either
      (Left . map (prettyError "built.yaml") . NE.toList)
      (const (Right ()))
      (decodeDocument @Value "" built)
  assertEqual
    "decoder error in a built node"
    (Just (0, 0, "expected an integer, but got a string"))
    (errorOf (decodeDocument @Int "" (S.document (S.plainNode "x"))))
