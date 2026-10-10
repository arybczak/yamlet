module Yamlet.Test.Decode.Limits
  ( test_nesting
  , test_aliasKeys
  , test_aliasLimit
  , test_tagPrefixLimit
  , test_longNumbers
  , test_longUnknownNames
  , test_manyKeys
  , test_nestedDuplicates
  , test_manyErrors
  , test_deepErrors
  ) where

import Data.Either
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Ratio
import Data.Scientific qualified as Sci
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Tasty.HUnit

import Yamlet
import Yamlet.Syntax qualified as S
import Yamlet.Test.Decode.Helpers
import Yamlet.Test.Helpers

-- | The time to parse nested flow sequences is linear in the depth.
test_nesting :: Assertion
test_nesting = do
  let nested :: Int -> T.Text -> T.Text
      nested d t = T.replicate d "[" <> t <> T.replicate d "]"
      depth :: Value -> Int
      depth = \case
        Sequence [x] -> 1 + depth x
        Mapping [(k, _)] -> depth k
        _ -> 0
  assertEqual
    "sequences"
    (Right 100000)
    (depth <$> decodeText (nested 100000 "x"))
  assertEqual
    "key"
    (Right 101)
    (depth <$> decodeText ("[" <> nested 100 "x" <> ": y]"))
  assertBool "key on two lines" (isLeft (decodeText @Value "[[a,\n b]: c]"))
  -- A flow sequence at the start of a line is first tried as a key.
  assertEqual
    "on two lines"
    (Right 40)
    (depth <$> decodeText (nested 40 "x\n"))
  assertEqual
    "block sequences on a long line"
    (Right 40000)
    (depth <$> decodeText (T.replicate 40000 "- " <> T.replicate 1000000 "x"))
  assertEqual
    "block sequences on a line with a comment below"
    (Right 200000)
    (depth <$> decodeText (T.replicate 200000 "- " <> "x\n\n# c\n"))
  assertEqual
    "block sequences on an indented line with a comment above"
    (Right 400000)
    $ depth
      <$> decodeText
        ("# c\n" <> T.replicate 400000 " " <> T.replicate 400000 "- " <> "x\n")
  assertEqual
    "block sequences with empty lines below"
    (Right 20000)
    (depth <$> decodeText (T.replicate 20000 "- " <> "x\n" <> T.replicate 20000 "\n"))
  assertEqual
    "flow sequences with empty lines inside"
    (Right 20000)
    (depth <$> decodeText (nested 20000 ("x" <> T.replicate 20000 "\n")))

-- | The check for duplicate keys compares keys with aliases correctly.
test_aliasKeys :: Assertion
test_aliasKeys = do
  let check
        :: String
        -> Maybe ((Int, Int, String), (Int, Int, String))
        -> T.Text
        -> Assertion
      check preface expected keys =
        assertEqual
          preface
          expected
          (errorWithNote (decodeAllText @Value (laughs 3 <> keys)))
  check
    "different keys"
    Nothing
    "? *a3\n: 1\n? [*a2, 1]\n: 2\n? [*a2, 2]\n: 3\n"
  check
    "duplicate key"
    (Just ((7, 3, "duplicate key"), (5, 3, "the first key")))
    "? [*a3, 1]\n: 1\n? [*a3, 1]\n: 2\n"
  check
    "duplicate alias key"
    (Just ((7, 3, "duplicate key *a3"), (5, 3, "the first key *a3")))
    "? *a3\n: 1\n? *a3\n: 2\n"
  -- Keys from two separate chains of anchors are equal only after an
  -- expansion to 2^12 items.
  let chains :: T.Text -> T.Text -> T.Text
      chains x y =
        T.unlines $
          ["- &a0 [" <> x <> "]", "- &b0 [" <> y <> "]"]
            ++ [ T.pack
                   ("- &" ++ c : show i ++ " [*" ++ c : show (i - 1) ++ ", *" ++ c : show (i - 1) ++ "]")
               | i <- [1 .. 12 :: Int]
               , c <- "ab"
               ]
            ++ ["- ? *a12", "  : 1", "  ? *b12", "  : 2"]
  assertEqual
    "equal chains"
    ( Just
        ( (29, 5, "duplicate key *b12, the same value as the first key")
        , (27, 5, "the first key *a12")
        )
    )
    (errorWithNote (decodeAllText @Value (chains "x" "x")))
  assertEqual
    "different chains"
    Nothing
    (errorWithNote (decodeAllText @Value (chains "x" "y")))

-- | Aliases can add 100000 visits to a traversal of a small document, and as
-- many visits as the document has to a large one. Each node and each
-- character of its scalar, tag and anchor is a visit.
test_aliasLimit :: Assertion
test_aliasLimit = do
  assertEqual
    "small expansion"
    Nothing
    (errorOf (decodeAllText @Value (laughs 3)))
  assertEqual
    "exponential expansion"
    (Just (5, 25, "the aliases add more than 100000 nodes and characters"))
    (errorOf (decodeAllText @Value (laughs 9)))
  let items = T.intercalate ", " (replicate 200000 "x")
      copies :: Int -> T.Text
      copies k = T.unlines ("- &a [" <> items <> "]" : replicate k "- *a")
  assertEqual
    "large document with one copy"
    Nothing
    (errorOf (decodeAllText @Value (copies 1)))
  assertEqual
    "large document with two copies"
    (Just (3, 3, "the aliases add more than 400005 nodes and characters"))
    (errorOf (decodeAllText @Value (copies 2)))
  let long = T.replicate 100000 "x"
      textCopies :: Int -> T.Text
      textCopies k = T.unlines ("- &a " <> long : replicate k "- *a")
  assertEqual
    "long scalar with one copy"
    Nothing
    (errorOf (decodeAllText @Value (textCopies 1)))
  assertEqual
    "long scalar with many copies"
    (Just (3, 3, "the aliases add more than 101003 nodes and characters"))
    (errorOf (decodeAllText @Value (textCopies 1000)))
  let tagCopies :: Int -> T.Text
      tagCopies k = T.unlines ("- &a !" <> long <> " x" : replicate k "- *a")
  assertEqual
    "long tag with one copy"
    Nothing
    (errorOf (decodeAllText @Value (tagCopies 1)))
  assertEqual
    "long tag with many copies"
    (Just (3, 3, "the aliases add more than 101005 nodes and characters"))
    (errorOf (decodeAllText @Value (tagCopies 1000)))
  let anchorCopies :: Int -> T.Text
      anchorCopies k = T.unlines ("- &a [&" <> long <> " x]" : replicate k "- *a")
  assertEqual
    "long anchor inside with one copy"
    Nothing
    (errorOf (decodeAllText @S.Node (anchorCopies 1)))
  assertEqual
    "long anchor inside with many copies"
    (Just (3, 3, "the aliases add more than 101005 nodes and characters"))
    (errorOf (decodeAllText @S.Node (anchorCopies 1000)))
  -- The documents of a stream share the limit.
  let stream :: Int -> T.Text
      stream k = T.concat (replicate k ("---\n" <> laughs 3))
  assertEqual
    "four documents of a stream"
    Nothing
    (errorOf (decodeAllText @Value (stream 4)))
  assertEqual
    "five documents of a stream"
    (Just (25, 15, "the aliases add more than 100000 nodes and characters"))
    (errorOf (decodeAllText @Value (stream 5)))
  assertEqual
    "five documents of a stream with one document expected"
    (Just (25, 15, "the aliases add more than 100000 nodes and characters"))
    (errorOf (decodeText @Value (stream 5)))
  case S.parseDocumentsText (stream 5) of
    Right docs -> do
      assertEqual
        "five parsed documents"
        (Just (25, 15, "the aliases add more than 100000 nodes and characters"))
        (errorOf (decodeDocuments @Value (stream 5) docs))
      assertEqual
        "a parsed document on its own"
        Nothing
        (errorOf (traverse (decodeDocument @Value (stream 5)) docs))
    Left err -> assertFailure (show err)

-- | The prefixes of %TAG directives can add 100000 bytes to the tags of a
-- small input, and as many bytes as the input has to a large one.
test_tagPrefixLimit :: Assertion
test_tagPrefixLimit = do
  let uses :: T.Text -> Int -> T.Text
      uses prefix k = T.unlines ("%TAG !e! " <> prefix : "---" : replicate k "- !e!a 1")
  assertEqual
    "short prefix"
    Nothing
    (errorOf (decodeText @[Value] (uses "tag:x:" 1000)))
  let long = "tag:" <> T.replicate 100000 "x" <> ":"
  assertEqual
    "long prefix with one use"
    Nothing
    (errorOf (decodeText @[Value] (uses long 1)))
  assertEqual
    "long prefix with two uses"
    ( Just
        ( 4
        , 3
        , "the prefixes of %TAG directives add more than 100037 bytes to the tags"
        )
    )
    (errorOf (decodeText @[Value] (uses long 2)))
  -- Each tag adds 18 bytes, more than the 10 bytes of its line.
  assertEqual
    "default prefix"
    Nothing
    (errorOf (decodeText @[T.Text] (T.unlines (replicate 20000 "- !!str a"))))

-- | Anchors a0 to ak, where each anchor after a0 has ten aliases to the one
-- before it, and the alias *ak expands to about 10^(k+1) nodes.
laughs :: Int -> T.Text
laughs k =
  T.unlines $
    "a0: &a0 [x, x, x, x, x, x, x, x, x, x]"
      : [ T.pack $
            "a"
              ++ show i
              ++ ": &a"
              ++ show i
              ++ " ["
              ++ L.intercalate ", " (replicate 10 ("*a" ++ show (i - 1)))
              ++ "]"
        | i <- [1 .. k]
        ]

-- | The time to read a number is not quadratic in the number of its digits.
test_longNumbers :: Assertion
test_longNumbers = do
  let nines :: Int -> T.Text
      nines k = T.replicate k "9"
  assertEqual
    "integer"
    (Right (10 ^ (1000000 :: Int) - 1))
    (decodeText @Integer (nines 1000000))
  assertEqual
    "hexadecimal"
    (Right (16 ^ (100 :: Int) - 1))
    (decodeText @Integer ("0x" <> T.replicate 100 "f"))
  assertEqual
    "octal"
    (Right (8 ^ (100 :: Int) - 1))
    (decodeText @Integer ("0o" <> T.replicate 100 "7"))
  assertEqual
    "float"
    (Right (Float (Finite (Sci.scientific (10 ^ (1000000 :: Int) - 1) (-999999)))))
    (decodeText @Value ("9." <> nines 999999))
  assertEqual
    "exponent"
    ( Just
        ( 1
        , 1
        , "the exponent of the number is out of the range from -1000 to 1000, quote the value if it is a string, e.g. '1e"
            ++ T.unpack (nines 1000000)
            ++ "'"
        )
    )
    (errorOf (decodeText @Value ("1e" <> nines 1000000)))
  let zeros = T.replicate 300000 "0"
  assertEqual
    "trailing zeros"
    ( Just
        (
          ( 1
          , 600018
          , "duplicate key 0.1" ++ T.unpack zeros ++ "0, the same value as the first key"
          )
        , (1, 2, "the first key 0.1" ++ T.unpack zeros)
        )
    )
    . errorWithNote
    . decodeAllText @Value
    $ "{0.1" <> zeros <> ": a, 0.5" <> zeros <> ": b, 0.1" <> zeros <> "0: c}"
  -- The gcd of a reduction takes quadratic time for most types.
  let big = 3 ^ (1000000 :: Int) :: Integer
  assertEqual
    "fraction"
    (Right big)
    $ numerator
      <$> decodeText @Rational
        ( "{numerator: "
            <> T.pack (show big)
            <> ", denominator: "
            <> T.pack (show @Integer (7 ^ (600000 :: Int)))
            <> "}"
        )
  assertEqual
    "float with a long integer part"
    (Right (Float (Finite (Sci.scientific (10 ^ (1000000 :: Int) - 1) (-999000)))))
    (decodeText @Value (nines 1000 <> "." <> nines 999000))

-- | The search for a close known name does not compute the distance of a
-- long unknown name to each known name.
test_longUnknownNames :: Assertion
test_longUnknownNames = do
  let name = T.replicate 1000000 "a"
  assertEqual
    "value"
    (Just (1, 1, "unknown value " ++ show name ++ ", expected one of: small, large, 10"))
    (errorOf (decodeText @Size name))
  assertEqual
    "key"
    (Just (2, 3, "unknown key " ++ show name ++ ", expected one of: name, paths, jobs"))
    (errorOf (decodeText @Config ("name: x\n? " <> name <> "\n: 1\n")))

-- | The time of the check for duplicate keys is not quadratic in the number
-- of keys.
test_manyKeys :: Assertion
test_manyKeys = do
  let keys :: [T.Text]
      keys = [T.pack ("k" ++ show i) | i <- [1 .. 30000 :: Int]]
      count :: [T.Text] -> Either (NE.NonEmpty Error) Int
      count ks = length . entries <$> decodeText @Value (T.unlines (map (<> ": 1") ks))
  assertEqual
    "one collection key"
    (Right 30001)
    (count ("[c]" : keys))
  assertEqual
    "collection keys"
    (Right 30000)
    (count (map (\k -> "[" <> k <> "]") keys))
  assertEqual
    "mapping keys"
    (Right 30000)
    (count (map (\k -> "{a: " <> k <> "}") keys))
  let large = "{" <> T.intercalate ", " (map (<> ": 1") keys) <> "}"
  assertEqual
    "large equal keys"
    (Just ((3, 3, "duplicate key"), (1, 3, "the first key")))
    . errorWithNote
    $ decodeAllText @Value ("? " <> large <> "\n: 1\n? " <> large <> "\n: 2\n")
  let deep = nestedKey 14 "0"
  assertEqual
    "nested equal keys"
    (Just ((3, 3, "duplicate key"), (1, 3, "the first key")))
    . errorWithNote
    $ decodeAllText @Value ("? " <> deep <> "\n: 1\n? " <> deep <> "\n: 2\n")
  where
    -- Two mappings as keys that differ only in their last value.
    nestedKey :: Int -> T.Text -> T.Text
    nestedKey d v
      | d == 0 = v
      | otherwise =
          "{"
            <> nestedKey (d - 1) "0"
            <> ": 1, "
            <> nestedKey (d - 1) "1"
            <> ": "
            <> v
            <> "}"

    entries :: Value -> [(Value, Value)]
    entries = \case
      Mapping kvs -> kvs
      _ -> []

newtype NestedMap = NestedMap (M.Map T.Text NestedMap)
  deriving newtype (FromYaml)

newtype NestedSet = NestedSet (Set.Set NestedSet)
  deriving stock (Eq, Ord)
  deriving newtype (FromYaml)

-- | The time of the check for duplicates is linear in the depth of
-- collections that each have a duplicate.
test_nestedDuplicates :: Assertion
test_nestedDuplicates = do
  let depth = 1000
      maps :: Int -> T.Text
      maps d
        | d == 0 = "{}"
        | otherwise = "{a: {}, !x a: {}, b: " <> maps (d - 1) <> "}"
      sets :: Int -> T.Text
      sets d
        | d == 0 = "[[[]]]"
        | otherwise = "[[], [], " <> sets (d - 1) <> "]"
  assertEqual
    "maps"
    ( concat
        (replicate depth ["duplicate key \"a\" after conversion", "the first key \"a\""])
    )
    (map (\(_, _, msg) -> msg) (errorsOf (decodeText @NestedMap (maps depth))))
  assertEqual
    "sets"
    (concat (replicate depth ["duplicate element", "the first element"]))
    (map (\(_, _, msg) -> msg) (errorsOf (decodeText @NestedSet (sets depth))))

-- | The time to locate errors and to find their paths is linear in the number
-- of errors, also for errors on one line.
test_manyErrors :: Assertion
test_manyErrors = do
  let n = 100000 :: Int
  check
    "flow"
    ("[" <> T.intercalate ", " (replicate n "x") <> "]")
    (\i -> 1 + 3 * i)
    (\i -> (1, 2 + 3 * i))
  check
    "block"
    (T.concat (replicate n "- x\n"))
    (\i -> 2 + 4 * i)
    (\i -> (i + 1, 3))
  where
    check :: String -> T.Text -> (Int -> Int) -> (Int -> (Int, Int)) -> Assertion
    check preface input offset location = case S.parseDocumentsText input of
      Right [doc] -> do
        let n = length (items doc.root)
            offs = [Offset (offset i) | i <- [0 .. n - 1]]
            errs = errorsAt input [(o, "e") | o <- offs]
        assertEqual
          (preface ++ ", locations")
          [location i | i <- [0 .. n - 1]]
          [(err.location.line, err.location.column) | err <- errs]
        -- The time to render an error does not depend on the length of its
        -- line.
        assertEqual
          (preface ++ ", rendered")
          n
          (length (filter (elem '^') (map (prettyError "f") errs)))
        assertEqual
          (preface ++ ", paths")
          [[Index i] | i <- [0 .. n - 1]]
          (map pathElements (nodePaths offs doc.root))
      _ -> assertFailure "expected one document"

    items :: S.Node -> [S.Node]
    items node = case node.content of
      S.SequenceContent _ xs -> xs
      _ -> []

newtype NestedList = NestedList [NestedList]
  deriving newtype (FromYaml)

-- | The time and the memory of the paths of many errors deep in a document
-- are linear in its size.
test_deepErrors :: Assertion
test_deepErrors = do
  let n = 20000
      input =
        T.replicate n "[" <> T.intercalate ", " (replicate n "x") <> T.replicate n "]"
  case decodeText @NestedList input of
    Left errs -> do
      assertEqual
        "errors"
        n
        (length errs)
      assertEqual
        "depth"
        n
        (length (pathElements (NE.last errs).path))
    Right _ -> assertFailure "expected errors"
