{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- Full laziness would float the input out of the action of a check into the
-- list of checks, which keeps it alive.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | The checks run without tasty. In a test of tasty, a major collection at
-- times kept the input of a decode alive although no value referred to it,
-- also after the value was dropped, and ghc-debug found no path from the
-- roots to the input.
module Main (main) where

import Control.Exception
import Control.Monad
import Data.Functor.Const
import Data.Functor.Identity
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid qualified as Mon
import Data.Ord
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
import GHC.Exts (mkWeakNoFinalizer#)
import GHC.Generics
import GHC.IO
import GHC.Weak
import System.Exit
import System.IO
import System.Mem

import Yamlet
import Yamlet.Test.Thunks

-- | Run the checks, and fail if one of them fails.
main :: IO ()
main = do
  failures <- fmap catMaybes . forM checks $ \(Check name act) -> do
    result <- act
    putStrLn $ name ++ ": " ++ maybe "OK" (const "FAIL") result
    pure $ (\msg -> name ++ ": " ++ msg) <$> result
  unless (null failures) $ do
    mapM_ (hPutStrLn stderr) failures
    exitFailure

-- | A check with its name. The action returns the message of a failure.
data Check = Check String (IO (Maybe String))

-- | A decoded value does not keep the input alive, for every instance of the
-- library, and neither do the errors of a failed decode. A text that refers
-- to the input keeps all of it, e.g. a slice of it, or a thunk that would
-- copy a slice.
checks :: [Check]
checks =
  [ Check "input without a value" $ do
      input <- evaluate (T.copy "a")
      weak <- weakArray input
      performMajorGC
      kept <- isJust <$> deRefWeak weak
      pure $ if kept then Just "the check keeps the input alive" else Nothing
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
  , errorRetains @(M.Map T.Text T.Text) "error with a key in the path" "k: [1]"
  , errorRetains @(T.Text, M.Map T.Text T.Text)
      "error with an alias in the path"
      "- &a k\n- *a : [1]"
  , errorRetains @Closed "error with a key in the message" "title: a\nhots: 1"
  ]

-- | The array of the input is garbage while the value is alive. A weak
-- pointer tells, unlike the size of the heap.
retains :: forall a. FromYaml a => String -> T.Text -> Check
retains name doc = Check name $ do
  -- A copy, because the array of a literal is never garbage.
  input <- evaluate (T.copy doc)
  weak <- weakArray input
  case decodeText @a input of
    Left errs -> pure (Just (show errs))
    Right v -> do
      ref <- newIORef v
      performMajorGC
      kept <- isJust <$> deRefWeak weak
      failure <-
        if kept
          then Just <$> (keptAlive "the value" weak =<< readIORef ref)
          else pure Nothing
      _ <- evaluate =<< readIORef ref
      pure failure

-- | The array of the input is garbage while the errors of a failed decode
-- are alive.
errorRetains :: forall a. FromYaml a => String -> T.Text -> Check
errorRetains name doc = Check name $ do
  input <- evaluate (T.copy doc)
  weak <- weakArray input
  case decodeText @a input of
    Left errs -> do
      -- An error in weak head normal form has no thunks that keep the input.
      mapM_ evaluate errs
      ref <- newIORef errs
      performMajorGC
      kept <- isJust <$> deRefWeak weak
      failure <-
        if kept
          then Just <$> (keptAlive "the errors" weak =<< readIORef ref)
          else pure Nothing
      _ <- evaluate =<< readIORef ref
      pure failure
    Right _ -> pure (Just "the decode succeeded")

-- | The message for a value that keeps the input alive, with what tells a
-- leak from the state of the runtime: whether a second collection frees the
-- input while the value is still alive, and the thunks in the value. The
-- failure is rare, so the message has to tell all there is.
keptAlive :: String -> Weak () -> a -> IO String
keptAlive what weak x = do
  ts <- thunks x
  performMajorGC
  still <- isJust <$> deRefWeak weak
  _ <- evaluate x
  pure $
    what
      ++ " keeps the input alive; after a second collection, the input is "
      ++ (if still then "still alive" else "gone")
      ++ "; thunks in the value: "
      ++ (if null ts then "none" else L.intercalate ", " ts)

-- | A weak pointer to the array of the text. A slice of the text shares the
-- array, so the weak pointer is empty only if no text of the array is alive.
weakArray :: T.Text -> IO (Weak ())
weakArray (T.Text (A.ByteArray arr) _ _) = IO $ \s -> case mkWeakNoFinalizer# arr () s of
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

newtype Closed = Closed {title :: T.Text}
  deriving stock (Generic)
  deriving (FromYaml) via GenericYaml Closed

instance GenericYamlOptions Closed where
  yamlOptions = defaultYamlOptions {rejectUnknownFields = True}

newtype Keys = Keys [T.Text]

-- The list is lazy, and its unevaluated rest would keep the object.
instance FromYaml Keys where
  parseYaml = withMapping $ \o ->
    let keys = objectKeys o in length keys `seq` pure (Keys keys)

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
