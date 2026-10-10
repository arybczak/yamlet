module Yamlet.Test.Decode.Errors
  ( test_typeErrors
  , test_collectedErrors
  , test_keyErrors
  , test_errorPaths
  , prop_errorsAt
  , test_nodePaths
  , test_prettyError
  ) where

import Control.Monad
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.Fixed
import Data.Foldable
import Data.Int
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Ratio
import Data.Scientific qualified as Sci
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.UUID.Types qualified as UUID
import Data.Void
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Fixed)

import Yamlet
import Yamlet.Syntax qualified as S
import Yamlet.Test.Decode.Helpers
import Yamlet.Test.Helpers

-- | A value written as an empty mapping.
data EmptyDir = EmptyDir
  deriving stock (Eq, Show)

instance FromYaml EmptyDir where
  parseYaml = withMapping $ \o -> EmptyDir <$ rejectUnknownKeys [] o

newtype IntOrText = IntOrText (Either Integer T.Text)
  deriving stock (Eq, Show)

instance FromYaml IntOrText where
  parseYaml n =
    IntOrText
      <$> ((Left <$> withInt pure n) `orElse` (Right <$> withText pure n))

-- | A choice from a list that the program found empty.
newtype Profile = Profile Int
  deriving stock (Eq, Show)

instance FromYaml Profile where
  parseYaml = oneOf []

test_typeErrors :: Assertion
test_typeErrors = do
  assertEqual
    "first alternative"
    (Right (IntOrText (Left 1)))
    (decodeText "1")
  assertEqual
    "second alternative"
    (Right (IntOrText (Right "a")))
    (decodeText "a")
  assertEqual
    "error of the second alternative"
    (Just (1, 1, "expected a string, but got a boolean, quote the value, e.g. 'true'"))
    (errorOf (decodeText @IntOrText "true"))
  assertEqual
    "known name"
    (Right (Size 2))
    (decodeText "large")
  assertEqual
    "close name"
    (Just (1, 1, "unknown value \"lage\", did you mean \"large\"?"))
    (errorOf (decodeText @Size "lage"))
  assertEqual
    "other name"
    (Just (1, 1, "unknown value \"medium\", expected one of: small, large, 10"))
    (errorOf (decodeText @Size "medium"))
  assertEqual
    "plain name that is not a string"
    (Just (1, 1, "expected a string, but got an integer, quote the value, e.g. '10'"))
    (errorOf (decodeText @Size "10"))
  assertEqual
    "collection"
    (Just (1, 1, "expected one of: small, large, 10, but got a list"))
    (errorOf (decodeText @Size "[small]"))
  assertEqual
    "name without names"
    (Just (1, 1, "unknown value \"dev\", no value is accepted"))
    (errorOf (decodeText @Profile "dev"))
  assertEqual
    "collection without names"
    (Just (1, 1, "no value is accepted"))
    (errorOf (decodeText @Profile "[dev]"))
  assertEqual
    "pair"
    (Just (1, 1, "expected a list of 2 elements, but got 1"))
    (errorOf (decodeText @(Int, Int) "[1]"))
  assertEqual
    "triple"
    (Just (1, 1, "expected a list of 3 elements, but got 4"))
    (errorOf (decodeText @(Int, Int, Int) "[1, 2, 3, 4]"))
  assertEqual
    "unit from null"
    (Just (1, 1, "expected an empty list, but got null"))
    (errorOf (decodeText @() "null"))
  assertEqual
    "unit from a list with items"
    (Just (1, 1, "expected an empty list, but got a list"))
    (errorOf (decodeText @() "[1]"))
  assertEqual
    "second document"
    (Just (3, 1, "expected a single document, but got a second one"))
    (errorOf (decodeText @T.Text "a\n---\nb\n"))
  assertEqual
    "YAML 1.1 boolean"
    ( Just
        ( 1
        , 1
        , "expected a boolean, but got the string \"yes\", which is a boolean only in YAML 1.1, use true or false"
        )
    )
    (errorOf (decodeText @Bool "yes"))
  assertEqual
    "quoted YAML 1.1 boolean"
    (Just (1, 1, "expected a boolean, but got a string"))
    (errorOf (decodeText @Bool "'yes'"))
  assertEqual
    "YAML 1.1 boolean with a string tag"
    (Just (1, 7, "expected a boolean, but got a string"))
    (errorOf (decodeText @Bool "!!str yes"))
  assertEqual
    "YAML 1.1 boolean with a tag"
    ( Just
        ( 1
        , 11
        , "invalid value for the tag !!bool, \"off\" is a boolean only in YAML 1.1"
        )
    )
    (errorOf (decodeAllText @Value "a: !!bool off\n"))
  assertEqual
    "list instead of string"
    (Just (1, 7, "expected a string, but got a list"))
    (errorOf (decodeText @Config "name: [a]\n"))
  assertEqual
    "number instead of list"
    (Just (2, 8, "expected a list, but got an integer"))
    (errorOf (decodeText @Config "name: x\npaths: 42\n"))
  assertEqual
    "element of a list"
    (Just (2, 12, "expected a string, but got a boolean, quote the value, e.g. 'true'"))
    (errorOf (decodeText @Config "name: x\npaths: [a, true]\n"))
  assertEqual
    "float instead of string"
    ( Just
        ( 1
        , 1
        , "expected a string, but got a floating-point number, quote the value, e.g. '9.10'"
        )
    )
    (errorOf (decodeText @T.Text "9.10"))
  assertEqual
    "integer instead of string"
    (Just (1, 1, "expected a string, but got an integer, quote the value, e.g. '007'"))
    (errorOf (decodeText @T.Text "007"))
  assertEqual
    "empty value instead of string"
    (Just (1, 6, "expected a string, but got null"))
    (errorOf (decodeText @Config "name:\n"))
  assertEqual
    "null instead of string"
    (Just (1, 1, "expected a string, but got null, quote the value, e.g. 'null'"))
    (errorOf (decodeText @T.Text "null"))
  assertEqual
    "tilde instead of string"
    (Just (1, 1, "expected a string, but got null, quote the value, e.g. '~'"))
    (errorOf (decodeText @T.Text "~"))
  assertEqual
    "tagged integer instead of string"
    (Just (1, 7, "expected a string, but got an integer"))
    (errorOf (decodeText @T.Text "!!int 5"))
  assertEqual
    "out of range"
    (Just (1, 1, "the integer is out of the range from -128 to 127"))
    (errorOf (decodeText @Int8 "300"))
  assertEqual
    "custom failure"
    (Just (1, 5, "not a vowel"))
    (errorOf (decodeText @[Vowel] "[a, x]"))
  assertEqual
    "ordering"
    (Just (1, 1, "expected LT, EQ or GT"))
    (errorOf (decodeText @Ordering "lt"))
  assertEqual
    "uppercase UUID"
    (Right (UUID.fromWords 0x123e4567 0xe89b12d3 0xa4564266 0x14174000))
    (decodeText "123E4567-E89B-12D3-A456-426614174000")
  assertEqual
    "invalid UUID"
    (Just (1, 1, "expected a UUID such as 123e4567-e89b-12d3-a456-426614174000"))
    (errorOf (decodeText @UUID.UUID "123e4567e89b12d3a456426614174000"))
  assertEqual
    "void"
    (Just (1, 1, "the type Void has no values"))
    (errorOf (decodeText @Void "a"))
  assertEqual
    "zero denominator"
    (Just (1, 29, "the denominator is 0"))
    (errorOf (decodeText @Rational "{numerator: 1, denominator: 0}"))
  assertEqual
    "negative denominator"
    (Right (negate 1 % 2))
    (decodeText @Rational "{numerator: 2, denominator: -4}")
  assertEqual
    "negation of minBound"
    (Just (1, 1, "the fraction is out of the range of the type"))
    . errorOf
    $ decodeText @(Ratio Int) "{numerator: -9223372036854775808, denominator: -1}"
  assertEqual
    "minBound as the denominator"
    (Just (1, 1, "the fraction is out of the range of the type"))
    . errorOf
    $ decodeText @(Ratio Int) "{numerator: 1, denominator: -9223372036854775808}"
  assertEqual
    "minBound reduced"
    (Right (negate 4611686018427387904 % 1))
    (decodeText @(Ratio Int) "{numerator: -9223372036854775808, denominator: 2}")
  assertEqual
    "fixed from an integer"
    (Right 3)
    (decodeText @Centi "3")
  assertEqual
    "fixed with fewer digits"
    (Right 1.5)
    (decodeText @Centi "1.5")
  assertEqual
    "fixed with an exponent"
    (Right 120)
    (decodeText @Centi "1.2e2")
  assertEqual
    "fixed with too many digits"
    (Just (1, 1, "expected a multiple of 0.01"))
    (errorOf (decodeText @Centi "1.239"))
  assertEqual
    "fixed of whole numbers"
    (Just (1, 1, "expected a multiple of 1"))
    (errorOf (decodeText @Uni "1.5"))
  assertEqual
    "largest fixed"
    (Right (10 ^ (1000 :: Int)))
    (decodeText @Centi "1e1000")
  assertEqual
    "resolution of 2s and 5s"
    (Right (MkFixed 7))
    (decodeText @(Fixed Fortieths) "0.175")
  assertEqual
    "step of a resolution of 2s and 5s"
    (Just (1, 1, "expected a multiple of 0.025"))
    (errorOf (decodeText @(Fixed Fortieths) "0.01"))
  assertEqual
    "whole number for a resolution without a decimal form"
    (Right (MkFixed 6))
    (decodeText @(Fixed Thirds) "2")
  assertEqual
    "step of a resolution without a decimal form"
    (Just (1, 1, "expected a multiple of 1/3"))
    (errorOf (decodeText @(Fixed Thirds) "0.7"))
  assertEqual
    "fixed with a huge exponent"
    (Left "the exponent of the number is out of the range from -1000 to 1000")
    . first (snd . NE.head)
    $ runParser
      (parseYaml @Centi)
      (toYaml (Float (Finite (Sci.scientific 1 maxBound))))
  assertEqual
    "zero fixed with a huge exponent"
    (Right 0)
    (runParser (parseYaml @Centi) (toYaml (Float (Finite (Sci.scientific 0 maxBound)))))

newtype Vowel = Vowel Char

instance FromYaml Vowel where
  parseYaml = withText $ \t -> case T.unpack t of
    [c] | elem @[] c "aeiou" -> pure (Vowel c)
    _ -> fail "not a vowel"

-- | The applicative operators collect the errors of both parts, and '>>=' and
-- '>>' stop at the first error.
test_collectedErrors :: Assertion
test_collectedErrors = do
  assertEqual
    "fields"
    [ (1, 7, "expected a string, but got a list")
    , (2, 8, "expected a list, but got an integer")
    , (3, 7, "expected an integer, but got a string")
    ]
    (errorsOf (decodeText @Config "name: [x]\npaths: 1\njobs: x\n"))
  assertEqual
    "unknown keys"
    [ (2, 1, "unknown key \"job\", did you mean \"jobs\"?")
    , (3, 1, "unknown key \"bogus\", expected one of: name, paths, jobs")
    ]
    (errorsOf (decodeText @Config "name: x\njob: 1\nbogus: 2\n"))
  assertEqual
    "unknown keys that are not ASCII or do not print"
    [ (2, 1, "unknown key \"zażółć\", expected one of: name, paths, jobs")
    , (3, 1, "unknown key \"tab\\there\\x01\"")
    ]
    (errorsOf (decodeText @Config "name: x\nzażółć: 1\n\"tab\\there\\x01\": 2\n"))
  assertEqual
    "list of the known keys once"
    [ (2, 1, "unknown key \"foo\", expected one of: name, paths, jobs")
    , (3, 1, "unknown key \"bar\"")
    , (4, 1, "unknown key \"job\", did you mean \"jobs\"?")
    ]
    (errorsOf (decodeText @Config "name: x\nfoo: 1\nbar: 2\njob: 3\n"))
  assertEqual
    "no known keys"
    (Right EmptyDir)
    (decodeText @EmptyDir "{}")
  assertEqual
    "unknown keys without known keys"
    [(1, 1, "unknown key \"a\", the mapping must be empty"), (2, 1, "unknown key \"b\"")]
    (errorsOf (decodeText @EmptyDir "a: 1\nb: 2\n"))
  assertEqual
    "statement of a do block"
    [(2, 1, "unknown key \"bogus\", expected one of: name, paths, jobs")]
    (errorsOf (decodeText @Config "name: [x]\nbogus: 1\n"))
  assertEqual
    "items of a list"
    [ (1, 5, "expected an integer, but got a string")
    , (1, 11, "expected an integer, but got a string")
    ]
    (errorsOf (decodeText @[Int] "[1, x, 2, y]"))
  assertEqual
    "keys and values of a map"
    [ (1, 5, "expected an integer, but got a string")
    , (1, 8, "expected a string, but got an integer, quote the value, e.g. '1'")
    , (1, 17, "expected an integer, but got a string")
    ]
    (errorsOf (decodeText @(M.Map T.Text Int) "{a: x, 1: 2, b: y}"))
  assertEqual
    "fields of a fraction"
    [ (1, 12, "expected an integer, but got a string")
    , (2, 14, "expected an integer, but got a string")
    , (3, 1, "unknown key \"extra\", expected one of: numerator, denominator")
    ]
    (errorsOf (decodeText @Rational "numerator: x\ndenominator: y\nextra: 1\n"))
  assertEqual
    "fields of a calendar difference"
    [ (1, 9, "expected an integer, but got a string")
    , (2, 7, "expected an integer, but got a string")
    , (3, 1, "unknown key \"weeks\", expected one of: months, days")
    ]
    (errorsOf (decodeText @CalendarDiffDays "months: x\ndays: y\nweeks: 1\n"))
  assertEqual
    "duplicate keys of a map"
    [ (1, 8, "duplicate key 1.0 after conversion")
    , (1, 2, "the first key 1")
    , (1, 22, "duplicate key 2.0 after conversion")
    , (1, 16, "the first key 2")
    ]
    (errorsOf (decodeText @(M.Map Double T.Text) "{1: a, 1.0: b, 2: c, 2.0: d}"))
  assertEqual
    "duplicate elements of a set"
    [ (1, 5, "duplicate element 1.0")
    , (1, 2, "the first element 1")
    , (1, 13, "duplicate element 2.0")
    , (1, 10, "the first element 2")
    ]
    (errorsOf (decodeText @(Set.Set Double) "[1, 1.0, 2, 2.0]"))
  assertEqual
    "elements of a set"
    [ (1, 2, "expected a number, but got a string")
    , (1, 5, "expected a number, but got a string")
    ]
    (errorsOf (decodeText @(Set.Set Double) "[x, y]"))
  assertEqual
    "duplicate and invalid elements of a set"
    [ (1, 5, "duplicate element 1.0")
    , (1, 2, "the first element 1")
    , (1, 10, "expected a number, but got a string")
    ]
    (errorsOf (decodeText @(Set.Set Double) "[1, 1.0, x]"))
  assertEqual
    "duplicate elements of an int set"
    [ (1, 5, "duplicate element 0x1")
    , (1, 2, "the first element 1")
    , (1, 10, "duplicate element 1")
    , (1, 2, "the first element 1")
    ]
    (errorsOf (decodeText @IS.IntSet "[1, 0x1, 1]"))
  let count :: (S.Node -> Parser ()) -> Int
      count p =
        either
          (error . show)
          (either length (const 0) . runParser p)
          (decodeText @Node "[x, y]")
      item :: S.Node -> Parser Int
      item = parseNode parseYaml
      pair :: (Parser Int -> Parser Int -> Parser r) -> S.Node -> Parser ()
      pair op = withSequence $ \case
        [a, b] -> void (op (item a) (item b))
        _ -> fail "expected two items"
  assertEqual
    "traverse"
    2
    (count (withSequence (void . traverse item)))
  assertEqual
    "traverse_"
    2
    (count (withSequence (traverse_ item)))
  assertEqual
    "mapM_"
    1
    (count (withSequence (mapM_ item)))
  assertEqual
    "<*>"
    2
    (count (pair (\a b -> (,) <$> a <*> b)))
  assertEqual
    "*>"
    2
    (count (pair (*>)))
  assertEqual
    "<*"
    2
    (count (pair (<*)))
  assertEqual
    ">>"
    1
    (count (pair (>>)))
  assertEqual
    ">>="
    1
    (count (pair (\a b -> a >>= const b)))

test_keyErrors :: Assertion
test_keyErrors = do
  let merged = "base: &b\n  x: 1\nc:\n  <<: *b\n"
  assertEqual
    "value of a merge key"
    (Just (4, 7, "expected an integer, but got a mapping, merge keys are not supported"))
    (errorOf (decodeText @(M.Map T.Text (M.Map T.Text Int)) merged))
  assertEqual
    "unknown merge key"
    (Just (4, 3, "unknown key \"<<\", merge keys are not supported"))
    (errorOf (decodeText @[Config] "- &b\n  name: x\n  jobs: 2\n- <<: *b\n"))
  assertEqual
    "key missing next to a merge key"
    (Right (Left (pure (Offset 0, "missing key \"x\", merge keys are not supported"))))
    $ runParser (withMapping (\o -> parseField @Int o "x"))
      <$> decodeText @Node "<<: {x: 1}\n"
  assertEqual
    "two merge keys"
    ( Just
        ( (4, 3, "duplicate key \"<<\", merge keys are not supported")
        , (3, 3, "the first key \"<<\"")
        )
    )
    (errorWithNote (decodeAllText @Value "a: &a {x: 1}\nb:\n  <<: *a\n  <<: *a\n"))
  assertEqual
    "missing key"
    (Just (1, 1, "missing key \"name\""))
    (errorOf (decodeText @Config "jobs: 1\n"))
  assertEqual
    "unknown key"
    (Just (2, 1, "unknown key \"other\", expected one of: name, paths, jobs"))
    (errorOf (decodeText @Config "name: x\nother: 1\n"))
  assertEqual
    "key that is not a string"
    (Just (2, 1, "expected a string as the key, but got an integer"))
    (errorOf (decodeText @Config "name: x\n1: y\n"))
  assertEqual
    "unknown key close to a known one"
    (Just (2, 1, "unknown key \"job\", did you mean \"jobs\"?"))
    (errorOf (decodeText @Config "name: x\njob: 1\n"))
  let lookupError :: T.Text -> T.Text -> Maybe String
      lookupError key input =
        let parser = withMapping $ \o -> parseField @T.Text o key
        in case runParser parser <$> decodeText input of
             Right (Left ((_, msg) NE.:| [])) -> Just msg
             _ -> Nothing
  assertEqual
    "integer key"
    (Just "the key 404 is an integer, not a string")
    (lookupError "404" "200: ok\n404: not found\n")
  assertEqual
    "boolean key"
    (Just "the key true is a boolean, not a string")
    (lookupError "true" "true: 1\n")
  assertEqual
    "string key that is missing"
    (Just "missing key \"a\"")
    (lookupError "a" "b: 1\n")
  let withKeys :: [T.Text] -> T.Text
      withKeys ks = T.unlines $ map (<> ": 1") ks
  assertEqual
    "duplicate key"
    (Just ((3, 1, "duplicate key \"a\""), (1, 1, "the first key \"a\"")))
    (errorWithNote (decodeAllText @Value "a: 1\nb: 2\na: 3\n"))
  assertEqual
    "duplicate key with another text"
    ( Just
        ( (2, 1, "duplicate key ~, the same value as the first key")
        , (1, 1, "the first key null")
        )
    )
    (errorWithNote (decodeAllText @Value "null: 1\n~: 2\n"))
  assertEqual
    "duplicate string key with a tag"
    (Just ((2, 4, "duplicate key \"1\""), (1, 4, "the first key \"1\"")))
    (errorWithNote (decodeAllText @Value "!t 1: a\n!t 1: b\n"))
  assertEqual
    "duplicate among many scalar keys"
    (Just ((21, 1, "duplicate key \"k1\""), (1, 1, "the first key \"k1\"")))
    . errorWithNote
    . decodeAllText @Value
    $ T.unlines [T.pack ("k" ++ show i ++ ": 1") | i <- [1 .. 20 :: Int] ++ [1]]
  assertEqual
    "duplicate scalar key after a collection key"
    (Just ((3, 1, "duplicate key \"a\""), (1, 1, "the first key \"a\"")))
    (errorWithNote (decodeAllText @Value (withKeys ["a", "[b]", "a"])))
  assertEqual
    "duplicate collection key"
    (Just ((2, 1, "duplicate key"), (1, 1, "the first key")))
    (errorWithNote (decodeAllText @Value (withKeys ["{c: [d]}", "{c: [d]}"])))
  assertEqual
    "duplicate mapping key in another order"
    (Just ((2, 1, "duplicate key"), (1, 1, "the first key")))
    (errorWithNote (decodeAllText @Value (withKeys ["{a: 1, b: 2}", "{b: 2, a: 1}"])))

-- | A decoder error has the path to its node.
test_errorPaths :: Assertion
test_errorPaths = do
  check
    "nested key"
    (Right "hlint.version")
    (decodeText @(M.Map T.Text (M.Map T.Text T.Text)) "hlint:\n  version: 1\n")
  check
    "indices"
    (Right "[1][1]")
    (decodeText @[[Int]] "- [1]\n- [2, x]\n")
  -- The mapping and its first key start at the same place.
  check
    "missing key"
    (Right "[1]")
    (decodeText @[Config] "- name: x\n- jobs: 2\n")
  check
    "unknown key"
    (Right "[0]")
    (decodeText @[Config] "- name: x\n  bogus: 1\n")
  check
    "key in quotes"
    (Right "\"a.b\".c")
    (decodeText @(M.Map T.Text (M.Map T.Text Int)) "\"a.b\":\n  c: x\n")
  check
    "key with escapes"
    (Right "\"a\\nb\\t\\\"\\x07\\u2028\\U000e0001\"")
    (decodeText @(M.Map T.Text Int) "\"a\\nb\\t\\\"\\a\\L\\U000E0001\": x\n")
  check
    "inside a key"
    (Right "a")
    (decodeText @(M.Map T.Text (M.Map [Int] Int)) "a:\n  ? [1, x]\n  : 1\n")
  check
    "inside a key at the root"
    (Right "")
    (decodeText @(M.Map (M.Map T.Text Int) Int) "? {port: x}\n: 1\n")
  check
    "in the value of a collection key"
    (Right "?[1]")
    (decodeText @(M.Map [Int] [Int]) "? [1, 2]\n: [3, y]\n")
  check
    "string key ?"
    (Right "\"?\"[1]")
    (decodeText @(M.Map T.Text [Int]) "'?': [3, y]\n")
  check
    "alias key"
    (Right "*a[1]")
    (decodeText @(M.Map Value [Int]) "m: [&a 1]\n*a : [3, y]\n")
  check
    "string key like an alias"
    (Right "\"*a\"[1]")
    (decodeText @(M.Map T.Text [Int]) "'*a': [3, y]\n")
  assertEqual
    "path elements"
    (Left [CollectionKey, Index 1])
    $ first
      (pathElements . (.path) . NE.head)
      (decodeText @(M.Map [Int] [Int]) "? [1, 2]\n: [3, y]\n")
  check
    "empty value at the end of its key"
    (Right "a")
    (decodeText @(M.Map T.Text Int) "{a}")
  check
    "empty value at the end of an explicit key"
    (Right "a")
    (decodeText @(M.Map T.Text Int) "? a")
  check
    "empty key"
    (Right "")
    (decodeText @(M.Map Int Int) ": 1\n")
  check
    "duplicate key"
    (Right "a")
    (decodeText @Value "a:\n  b: 1\n  b: 2\n")
  check
    "root"
    (Right "")
    (decodeText @Int "x")
  let key = S.plainNode "a"
  check
    "built node"
    (Right "")
    (decodeDocument @(M.Map T.Text Int) "" (S.document (S.mappingNode [(key, key)])))
  where
    check :: String -> Either String String -> Either (NE.NonEmpty Error) a -> Assertion
    check preface expected r =
      assertEqual
        preface
        expected
        (either (Right . renderPath . (.path) . NE.head) (const (Left "no error")) r)

-- | 'errorsAt' gives the errors of 'errorAt', also for several errors on one
-- line and for offsets in any order.
prop_errorsAt :: Property
prop_errorsAt =
  forAll (T.pack <$> listOf (elements "ab \n\r\xFEFF\x17C\x1F600")) $ \input ->
    forAll (listOf (choose (-1, BS.length (T.encodeUtf8 input) + 1))) $ \offs ->
      let errs = [(Offset o, show o) | o <- offs]
      in errorsAt input errs === map (uncurry (errorAt input)) errs

-- | 'nodePaths' gives the paths of 'nodePath' for every offset of a document.
test_nodePaths :: Assertion
test_nodePaths = do
  let input = "a:\n  - [b, {c: d}]\n  - ? [e]\n    : f\nb: &x {g: h}\nc: *x\n"
  case S.parseDocumentsText input of
    Right [doc] -> do
      let offs = map Offset [-1 .. T.length input + 1]
      assertEqual
        "in order"
        (map (`nodePath` doc.root) offs)
        (nodePaths offs doc.root)
      assertEqual
        "in reverse"
        (map (`nodePath` doc.root) (reverse offs))
        (nodePaths (reverse offs) doc.root)
    r -> assertFailure (show r)

test_prettyError :: Assertion
test_prettyError = do
  case decodeText @Config "name: x\npaths: 42\n" of
    Left errs ->
      assertEqual
        "rendered"
        [expected]
        (map (prettyError "config.yaml") (NE.toList errs))
    Right _ -> assertFailure "expected an error"
  let longLine = "a: " <> T.replicate 100 "x" <> ": " <> T.replicate 100 "y" <> "\n"
  case decodeAllText @Value longLine of
    Left errs ->
      assertEqual
        "long line"
        [expectedLong]
        (map (prettyError "long.yaml") (NE.toList errs))
    Right _ -> assertFailure "expected an error"
  forM_ ([0 .. 90] ++ [160, 161, 170]) $ \n -> do
    let input = "\xFEFFk\n\xFEFF" <> T.pack (take n (cycle "aé\t€\x1F600")) <> "\r\nz"
        starts =
          [ i
          | (i, w) <- zip [0 ..] (BS.unpack (T.encodeUtf8 input))
          , w < 0x80 || w >= 0xC0
          ]
    forM_ starts $ \i -> do
      let err = errorAt input (Offset i) "m"
      assertEqual
        ("excerpt of a line of " ++ show n ++ " characters at " ++ show i)
        (excerpt err)
        (drop 1 (lines (prettyError "f" err)))
  case decodeText @(M.Map T.Text Int) "e\x301\&e\x301: x\n" of
    Left errs ->
      assertEqual
        "caret after combining marks"
        [["  | " ++ replicate 4 ' ' ++ "^"]]
        (map (drop 3 . lines . prettyError "f") (NE.toList errs))
    Right _ -> assertFailure "expected an error"
  where
    -- The excerpt and the caret from a scan of the whole line.
    excerpt :: Error -> [String]
    excerpt err =
      [ "  |"
      , show err.location.line ++ " | " ++ shown
      , "  | " ++ map (\c -> if c == '\t' then '\t' else ' ') (take before shown) ++ "^"
      ]
      where
        full :: String
        full = T.unpack err.sourceLine

        start :: Int
        start = max 0 (min (err.location.column - 1 - 40) (length full - 80))

        shown :: String
        shown
          | length full <= 80 = full
          | otherwise =
              (if start > 0 then "..." else "")
                ++ take 80 (drop start full)
                ++ (if start + 80 < length full then "..." else "")

        before :: Int
        before
          | length full <= 80 = err.location.column - 1
          | otherwise = (if start > 0 then 3 else 0) + err.location.column - 1 - start

    expectedLong :: String
    expectedLong =
      L.intercalate
        "\n"
        [ "long.yaml:1:104: unexpected ':', quote the value if it contains \": \""
        , "  |"
        , "1 | ..." ++ replicate 40 'x' ++ ": " ++ replicate 38 'y' ++ "..."
        , "  | " ++ replicate 43 ' ' ++ "^"
        ]

    expected :: String
    expected =
      L.intercalate
        "\n"
        [ "config.yaml:2:8: paths: expected a list, but got an integer"
        , "  |"
        , "2 | paths: 42"
        , "  |        ^"
        ]
