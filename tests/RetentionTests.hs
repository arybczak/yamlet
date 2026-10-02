{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- Full laziness would float the input out of the action of a test into the
-- test tree, which keeps it alive.
{-# OPTIONS_GHC -fno-full-laziness #-}

module RetentionTests (retentionTests) where

import Control.Exception
import Data.Functor.Const
import Data.Functor.Identity
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid qualified as Mon
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Semigroup qualified as Sem
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Internal qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Tree qualified as Tree
import GHC.Exts qualified as E
import GHC.Generics
import GHC.IO
import GHC.Weak
import System.Mem
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet

-- | A decoded value does not keep the input alive, for every instance of the
-- library. A text that refers to the input keeps all of it, e.g. a slice of
-- it, or a thunk that would copy a slice.
retentionTests :: TestTree
retentionTests =
  testGroup
    "retention"
    [ testCase "input without a value" $ do
        input <- evaluate (T.copy "a")
        weak <- weakArray input
        performMajorGC
        kept <- isJust <$> deRefWeak weak
        assertBool "the test keeps the input alive" (not kept)
    , retains @T.Text "Text" "a"
    , retains @TL.Text "lazy Text" "a"
    , retains @String "String" "a"
    , retains @(Maybe T.Text) "Maybe" "a"
    , retains @[T.Text] "list" "- a\n- b"
    , retains @(NE.NonEmpty T.Text) "NonEmpty" "- a\n- b"
    , retains @(Seq.Seq T.Text) "Seq" "- a\n- b"
    , retains @(Set.Set T.Text) "Set" "- a\n- b"
    , retains @(Tree.Tree T.Text) "Tree" "[a, []]"
    , retains @(M.Map T.Text T.Text) "Map" "a: b"
    , retains @(M.Map T.Text [T.Text]) "Map of lists" "a: [b]"
    , retains @(IM.IntMap T.Text) "IntMap" "1: b"
    , retains @IS.IntSet "IntSet" "[1, 2]"
    , retains @(T.Text, T.Text) "pair" "[a, b]"
    , retains @(T.Text, T.Text, T.Text) "triple" "[a, b, c]"
    , retains @(T.Text, T.Text, T.Text, T.Text) "quadruple" "[a, b, c, d]"
    , retains @(Either T.Text Int) "Left" "Left: a"
    , retains @(Either Int T.Text) "Right" "Right: a"
    , retains @[Commented T.Text] "Commented" "- a # c"
    , retains @[Located T.Text] "Located" "- a"
    , retains @[Identity T.Text] "Identity" "- a"
    , retains @[Const T.Text ()] "Const" "- a"
    , retains @[Down T.Text] "Down" "- a"
    , retains @[Sem.Min T.Text] "Min" "- a"
    , retains @[Sem.Max T.Text] "Max" "- a"
    , retains @[Sem.First T.Text] "Semigroup First" "- a"
    , retains @[Sem.Last T.Text] "Semigroup Last" "- a"
    , retains @[Mon.First T.Text] "Monoid First" "- a"
    , retains @[Mon.Last T.Text] "Monoid Last" "- a"
    , retains @[Sem.Dual T.Text] "Dual" "- a"
    , retains @[Sem.Sum Int] "Sum" "- 1"
    , retains @[Sem.Product Int] "Product" "- 1"
    , retains @[Sem.All] "All" "- true"
    , retains @[Sem.Any] "Any" "- true"
    , retains @[Ratio Int] "Ratio" "- {numerator: 1, denominator: 2}"
    , retains @[()] "unit" "- []"
    , retains @[Ordering] "Ordering" "- LT"
    , retains @[Proxy Int] "Proxy" "- null"
    , retains @[Day] "Day" "- 2026-01-01"
    , retains @[Value] "Value" "- !x {a: !y b}"
    , retains @[Node] "Node" "- !x {a: &y b} # c"
    , retains @Keys "objectKeys" "a: 1\nb: 2"
    , retains @[Choice] "oneOf" "- small"
    , retains @[Fields] "field lookups" "- a: x\n  b: y\n  c: [z]"
    , retains @[Mode] "enumeration" "- Development"
    , retains @[Endpoint] "record" "- host: a\n  tags: [b]"
    , retains @[Endpoint] "record with a default" "- tags: [b]"
    , retains @[Wrapped] "newtype" "- a"
    , retains @[Shape] "tagged record" "- tag: Circle\n  label: a"
    , retains @[Shape] "tagged constructor without fields" "- tag: Dot"
    , retains @[Move] "tagged contents" "- tag: Named\n  contents: a"
    , retains @[Step] "flat contents" "- tag: Ahead\n  name: a"
    , retains @[Figure] "single field record" "- Round:\n    label: a"
    , retains @[Figure] "single field contents" "- Sign: a"
    ]

-- | The array of the input is garbage while the value is alive. A weak
-- pointer tells, unlike the size of the heap, which the tests that run at
-- the same time change.
retains :: forall a. FromYaml a => String -> T.Text -> TestTree
retains name doc = testCase name $ do
  -- A copy, because the array of a literal is never garbage.
  input <- evaluate (T.copy doc)
  weak <- weakArray input
  case decodeText @a input of
    Left errs -> assertFailure (show errs)
    Right v -> do
      ref <- newIORef v
      performMajorGC
      kept <- isJust <$> deRefWeak weak
      _ <- evaluate =<< readIORef ref
      assertBool "the value keeps the input alive" (not kept)

-- | A weak pointer to the array of the text. A slice of the text shares the
-- array, so the weak pointer is empty only if no text of the array is alive.
weakArray :: T.Text -> IO (Weak ())
weakArray (T.Text (A.ByteArray arr) _ _) = IO $ \s -> case E.mkWeakNoFinalizer# arr () s of
  (# s', w #) -> (# s', Weak w #)

data Mode = Development | Production
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml) via GenericYaml Mode

data Endpoint = Endpoint {host :: T.Text, tags :: [T.Text]}
  deriving stock (Generic)
  deriving (FromYaml) via GenericYaml Endpoint

instance GenericYamlOptions Endpoint where
  yamlDefault = Just (Endpoint "localhost" [])

newtype Wrapped = Wrapped T.Text
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml) via GenericYaml Wrapped

data Shape = Circle {label :: T.Text} | Dot
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml) via GenericYaml Shape

data Move = Named T.Text | Stop
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml) via GenericYaml Move

newtype Inner = Inner {name :: T.Text}
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml) via GenericYaml Inner

data Step = Ahead Inner | Halt
  deriving stock (Generic)
  deriving (FromYaml) via GenericYaml Step

instance GenericYamlOptions Step where
  type SumEncoding Step = TaggedFlat

data Figure = Round {label :: T.Text} | Sign T.Text
  deriving stock (Generic)
  deriving (FromYaml) via GenericYaml Figure

instance GenericYamlOptions Figure where
  type SumEncoding Figure = SingleField

newtype Keys = Keys [T.Text]

-- The list is lazy, and its unevaluated rest would keep the object.
instance FromYaml Keys where
  parseYaml = withMapping $ \o -> let keys = objectKeys o in length keys `seq` pure (Keys keys)

newtype Choice = Choice Int

instance FromYaml Choice where
  parseYaml = oneOf [("small", Choice 1), ("large", Choice 2)]

data Fields = Fields (Maybe T.Text) T.Text (Maybe [T.Text])

instance FromYaml Fields where
  parseYaml = withMapping $ \o ->
    Fields
      <$> parseFieldMaybe o "a"
      <*> parseFieldDefault o "b" "x"
      <*> parseFieldIfPresent o "c"
