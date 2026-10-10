module Yamlet.Test.Generic (genericTests) where

import Control.Concurrent
import Control.Exception
import Data.Aeson qualified as A
import Data.Bifunctor
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import GHC.Conc
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet
import Yamlet.Syntax qualified as S
import Yamlet.Test.Helpers

genericTests :: TestTree
genericTests =
  testGroup
    "generic"
    [ testCase "record" test_record
    , testCase "types with a parameter" test_parameters
    , testCase "collected errors" test_collectedErrors
    , testCase "enumeration" test_enumeration
    , testCase "sum" test_sum
    , shapes
    , testCase "options" test_options
    , testCase "missing contents" test_missingContents
    , testCase "flat fields" test_flatten
    , testCase "single field" test_singleField
    , testCase "default" test_default
    , testCase "required field" test_requiredField
    , testCase "interrupted check of a default" test_interruptedDefault
    , testCase "modifiers" test_modifiers
    , testCase "commented fields" test_commentedFields
    , testCase "commented values" test_commentedValues
    , testProperty "snakeCase is camelTo2 of aeson" . forAll name $ \s ->
        snakeCase s === A.camelTo2 '_' s
    , testProperty "kebabCase is camelTo2 of aeson" . forAll name $ \s ->
        kebabCase s === A.camelTo2 '-' s
    ]
  where
    -- Mostly letters in both cases, where the rules matter.
    name :: Gen String
    name = listOf (elements "aAbBzZ1_-")

data Server = Server {host :: T.Text, port :: Int, tags :: Maybe [T.Text]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Server

-- | A type with a parameter. Its instances get the instances of the fields
-- as arguments, so GHC cannot inline them where it derives the instances.
data Pair a = Pair {left :: a, right :: a}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml (Pair a)

data Sparse a = Sparse {name :: T.Text, extra :: a}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml (Sparse a)

instance GenericYamlOptions (Sparse a) where
  yamlOptions = defaultYamlOptions {omitNullFields = True}

data Slot a = Filled a | Vacant
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml (Slot a)

data Turn = TurnLeft | TurnRight
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Turn

-- | The tags read as integers without quotes.
data Level = One | Two
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml Level

instance GenericYamlOptions Level where
  yamlOptions = defaultYamlOptions {constructorTagModifier = \case "One" -> "1"; _ -> "2"}

data Shape
  = Circle {radius :: Double}
  | Rectangle {width :: Double, height :: Double}
  | Dot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Shape

data Token = Label T.Text | Number Int | End
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Token

newtype Name = Name T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Name

-- | The constructors of the single-field encoding can mix their fields.
data Figure = Round {radius :: Double} | Named T.Text | Point
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Figure

instance GenericYamlOptions Figure where
  type SumEncoding Figure = SingleField

data Gauge = Gauge {level :: Int} | Off
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Gauge

instance GenericYamlOptions Gauge where
  type SumEncoding Gauge = SingleField

-- | A tag that is a boolean without quotes.
data Lamp = Dimmed Int | Dark
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml Lamp

instance GenericYamlOptions Lamp where
  type SumEncoding Lamp = SingleField
  yamlOptions = defaultYamlOptions {constructorTagModifier = \case "Dark" -> "false"; t -> t}

data Memo = Memo (Commented T.Text) | NoMemo
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Memo

instance GenericYamlOptions Memo where
  type SumEncoding Memo = SingleField

data Strict = Strict {size :: Int, note :: Maybe T.Text}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Strict

instance GenericYamlOptions Strict where
  yamlOptions = defaultYamlOptions {omitNullFields = True}

newtype Loose = Loose {size :: Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Loose

instance GenericYamlOptions Loose where
  yamlOptions = defaultYamlOptions {rejectUnknownFields = False}

-- | The name of the field reads as a boolean.
newtype Switch = Switch {true :: Maybe Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Switch

newtype DefaultSwitch = DefaultSwitch {true :: Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml DefaultSwitch

instance GenericYamlOptions DefaultSwitch where
  yamlDefault = Just (DefaultSwitch 0)

data Command = Forward {stepCount :: Int} | Stop
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Command

instance GenericYamlOptions Command where
  yamlOptions =
    defaultYamlOptions
      { tagKey = "command"
      , constructorTagModifier = map toLower
      , fieldLabelModifier = concatMap (\c -> if isUpper c then ['_', toLower c] else [c])
      }

data Reply = Answer (Maybe Int) | Silence
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Reply

-- The types of the flat encoding follow the example of tagged-json.
data Step
  = Ahead Distance
  | Rotate Direction
  | Accelerate Speed
  | Halt
  | -- Fields that do not merge.
    Wait Int
  | Again Step
  | Boxed Box
  | Packed Crate
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Step

instance GenericYamlOptions Step where
  type SumEncoding Step = TaggedFlat
  yamlOptions = defaultYamlOptions {tagKey = "step"}

data Crate = Crate {contents :: Int, size :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Crate

-- | The flat encoding with unknown keys ignored.
data Order = Hold Int | Hasten Speed
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Order

instance GenericYamlOptions Order where
  type SumEncoding Order = TaggedFlat
  yamlOptions = defaultYamlOptions {rejectUnknownFields = False}

-- | Another contents key.
data Volume = Level Int | Mute
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Volume

instance GenericYamlOptions Volume where
  yamlOptions = defaultYamlOptions {contentsKey = "value"}

-- | The flat encoding with another contents key, which is the key of a field.
data Parcel = Sent Speed | Held Int | Packaged Box
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Parcel

instance GenericYamlOptions Parcel where
  type SumEncoding Parcel = TaggedFlat
  yamlOptions = defaultYamlOptions {contentsKey = "speed"}

-- | The flat encoding of a mapping that an alias elsewhere can refer to.
data Shared = Shared Node | Unshared
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Shared

instance GenericYamlOptions Shared where
  type SumEncoding Shared = TaggedFlat

data Route = Route {first :: Shared, again :: Node}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Route

-- | The flat encoding of a field with a key close to the contents key.
data Event = Opened Issue | Closed
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Event

instance GenericYamlOptions Event where
  type SumEncoding Event = TaggedFlat

data Issue = Issue {title :: T.Text, comments :: [T.Text]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Issue

newtype Distance = Distance {distance :: Maybe Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Distance

newtype Speed = Speed {speed :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Speed

newtype Box = Box {contents :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Box

data Direction = Clockwise | Anticlockwise
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Direction

data Settings = Settings
  { name :: T.Text
  , retries :: Int
  , proxy :: Maybe T.Text
  , limits :: Limits
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Settings

instance GenericYamlOptions Settings where
  yamlDefault =
    Just
      Settings
        { name = "app"
        , retries = 3
        , proxy = Just "proxy"
        , limits = Limits 10 20
        }

data Limits = Limits {soft :: Int, hard :: Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Limits

instance GenericYamlOptions Limits where
  yamlDefault = Just (Limits 1 2)

data Mode = Fast {level :: Int} | Slow {level :: Int, delay :: Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Mode

instance GenericYamlOptions Mode where
  yamlDefault = Just (Slow 1 2)

data Job = Run Int | Skip
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Job

instance GenericYamlOptions Job where
  yamlDefault = Just (Run 3)

data Profile = Profile {user :: T.Text, proxy :: Maybe T.Text, note :: Maybe T.Text}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Profile

instance GenericYamlOptions Profile where
  yamlOptions = defaultYamlOptions {omitNullFields = True}
  yamlDefault = Just Profile {user = "app", proxy = Just "proxy", note = Nothing}

data Remark = Remark {user :: T.Text, note :: Commented (Maybe T.Text)}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Remark

instance GenericYamlOptions Remark where
  yamlOptions = defaultYamlOptions {omitNullFields = True}
  yamlDefault =
    Just (Remark "app" (Commented Nothing noComments {before = [Comment "default"]}))

data Account = Account {user :: T.Text, shell :: T.Text, home :: Maybe T.Text}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Account

instance GenericYamlOptions Account where
  yamlOptions = defaultYamlOptions {omitNullFields = True}
  yamlDefault =
    Just Account {user = requiredField, shell = "/bin/sh", home = requiredField}

data Task = Once Int | Never
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Task

instance GenericYamlOptions Task where
  yamlDefault = Just (Once requiredField)

data Login = Login {user :: !T.Text, shell :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Login

instance GenericYamlOptions Login where
  yamlDefault = Just (Login requiredField "/bin/sh")

newtype Port = Port {port :: Maybe Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml Port

instance GenericYamlOptions Port where
  yamlDefault = Just (Port requiredField)

-- | A default with a field that waits for a gate, so that a test can
-- interrupt the decoder while it checks the field.
data Gated = Gated {gated :: Int, other :: Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml Gated

instance GenericYamlOptions Gated where
  yamlDefault = Just (Gated gatedDefault requiredField)

gatedDefault :: Int
gatedDefault = unsafePerformIO (putMVar gateEntered () >> takeMVar gate >> pure 1)
-- Without the pragma, each use could evaluate the action again.
{-# NOINLINE gatedDefault #-}

gateEntered, gate :: MVar ()
gateEntered = unsafePerformIO newEmptyMVar
-- Without the pragmas, each use could get its own variable.
{-# NOINLINE gateEntered #-}
gate = unsafePerformIO newEmptyMVar
{-# NOINLINE gate #-}

-- | Records that keep the comments of their keys.
data Pipeline = Pipeline {name :: Commented T.Text, lint :: Commented Lint}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Pipeline

data Lint = Lint {version :: Commented T.Text, level :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Lint

-- | A record with a field whose type is its own field.
data Card = Card {title :: Title, pages :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Card

newtype Title = Title (Commented T.Text)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Title

-- | A comment above a key with no empty line below it belongs to the key, also
-- for the first key of a mapping.
test_commentedFields :: Assertion
test_commentedFields = do
  assertEqual
    "round trip"
    (Right input)
    (encodeText <$> decodeText @Pipeline input)
  let card = T.unlines ["# The title.", "title: Hello # short", "pages: 2"]
  assertEqual
    "field of a type that is its field"
    (Right card)
    (encodeText <$> decodeText @Card card)
  where
    input :: T.Text
    input =
      T.unlines
        [ "# The name of the pipeline."
        , "name: ci"
        , ""
        , "# The linter."
        , "lint: # optional"
        , "  # The version of the linter."
        , "  version: '3.8'"
        , "  level: warning"
        ]

data Setup = Setup {hooks :: Commented Hooks, name :: Commented T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Setup

newtype Hooks = Hooks {afterSetup :: Commented [Script]}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Hooks

instance GenericYamlOptions Hooks where
  yamlOptions = defaultYamlOptions {fieldLabelModifier = kebabCase}

newtype Script = Script {run :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Script

data Optional = Optional {first :: T.Text, extra :: Maybe (Commented Node)}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Optional

data Note = Note (Commented T.Text) | Blank
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Note

-- | The comments at the end of a collection and after a value.
test_commentedValues :: Assertion
test_commentedValues = do
  -- The comment at the top belongs to the root mapping, and a record has no
  -- place for it.
  assertEqual
    "round trip"
    (Right (T.unlines (drop 2 (T.lines input))))
    (encodeText <$> decodeText @Setup input)
  let optional =
        T.unlines ["first: a", "# The extra part.", "extra: # optional", "  x: 1"]
  assertEqual
    "optional field"
    (Right optional)
    (encodeText <$> decodeText @Optional optional)
  let note = T.unlines ["tag: Note", "# The text.", "contents: hello # c"]
  assertEqual
    "contents key"
    (Right note)
    (encodeText <$> decodeText @Note note)
  where
    -- The comment "trailing" is at the end of the mapping of hooks.
    input :: T.Text
    input =
      T.unlines
        [ "# top"
        , ""
        , "hooks: # k"
        , "  # above"
        , "  after-setup:"
        , "  - run: a"
        , "  # trailing"
        , "name: x # c"
        ]

-- The types that only the table of shapes uses.

data Unit = Unit
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Unit

data UnitTagged = UnitTagged
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml UnitTagged

instance GenericYamlOptions UnitTagged where
  yamlOptions = defaultYamlOptions {tagSingleConstructors = True}

newtype NameTagged = NameTagged T.Text
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml NameTagged

instance GenericYamlOptions NameTagged where
  yamlOptions = defaultYamlOptions {tagSingleConstructors = True}

newtype Single = Single {value :: Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Single

instance GenericYamlOptions Single where
  yamlOptions = defaultYamlOptions {tagSingleConstructors = True}

data Literal = Whole Int | Words T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Literal

data Motion = Go Distance | Hurry Speed
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Motion

instance GenericYamlOptions Motion where
  type SumEncoding Motion = TaggedFlat

newtype Bare = Bare {size :: Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Bare

instance GenericYamlOptions Bare where
  type SumEncoding Bare = SingleField

newtype Wrapped = Wrapped {size :: Int}
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Wrapped

instance GenericYamlOptions Wrapped where
  type SumEncoding Wrapped = SingleField
  yamlOptions = defaultYamlOptions {tagSingleConstructors = True}

data Light = Red | Green
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Light

instance GenericYamlOptions Light where
  type SumEncoding Light = SingleField

-- | Each supported shape of constructors, with the options that change its
-- encoding.
shapes :: TestTree
shapes =
  testGroup
    "shapes"
    [ shape
        "one constructor without fields"
        Unit
        "Unit\n"
    , shape
        "one constructor without fields, tagSingleConstructors"
        UnitTagged
        "UnitTagged\n"
    , shape
        "one field without a name"
        (Name "x")
        "x\n"
    , shape
        "one field without a name with a tag"
        (NameTagged "x")
        "tag: NameTagged\ncontents: x\n"
    , shape
        "one named field"
        (Speed 1)
        "speed: 1\n"
    , shape
        "named fields"
        Server {host = "a", port = 1, tags = Nothing}
        "host: a\nport: 1\ntags: null\n"
    , shape
        "named fields with a tag"
        (Single 1)
        "tag: Single\nvalue: 1\n"
    , shape
        "enumeration"
        TurnLeft
        "TurnLeft\n"
    , shape
        "fields without names"
        (Whole 1)
        "tag: Whole\ncontents: 1\n"
    , shape
        "fields without names, with fields"
        (Label "x")
        "tag: Label\ncontents: x\n"
    , shape
        "fields without names, without fields"
        End
        "tag: End\n"
    , shape
        "flat fields"
        (Go (Distance (Just 1)))
        "tag: Go\ndistance: 1\n"
    , shape
        "flat fields, with fields"
        (Ahead (Distance (Just 10)))
        "step: Ahead\ndistance: 10\n"
    , shape
        "flat fields, without fields"
        Halt
        "step: Halt\n"
    , shape
        "named fields in a sum, one field"
        (Fast 1)
        "tag: Fast\nlevel: 1\n"
    , shape
        "named fields in a sum, several fields"
        (Slow 1 2)
        "tag: Slow\nlevel: 1\ndelay: 2\n"
    , shape
        "named fields in a sum, with fields"
        (Rectangle 2 3)
        "tag: Rectangle\nwidth: 2.0\nheight: 3.0\n"
    , shape
        "named fields in a sum, without fields"
        Dot
        "tag: Dot\n"
    , shape
        "single field, named fields"
        (Round 1)
        "Round:\n  radius: 1.0\n"
    , shape
        "single field, a field without a name"
        (Named "x")
        "Named: x\n"
    , shape
        "single field, without fields"
        Point
        "Point\n"
    , shape
        "single field, one constructor"
        (Bare 1)
        "size: 1\n"
    , shape
        "single field, one constructor with a tag"
        (Wrapped 1)
        "Wrapped:\n  size: 1\n"
    , shape
        "single field, enumeration"
        Red
        "Red\n"
    ]
  where
    shape :: (Eq a, Show a, FromYaml a, ToYaml a) => String -> a -> T.Text -> TestTree
    shape preface x yaml = testCase preface $ do
      assertEqual
        "encoded"
        yaml
        (encodeText x)
      assertEqual
        "decoded"
        (Right x)
        (decodeText yaml)

test_record :: Assertion
test_record = do
  assertEqual
    "optional field"
    (Right Server {host = "a", port = 1, tags = Nothing})
    (decodeText "host: a\nport: 1\n")
  assertEqual
    "all fields"
    (Right Server {host = "a", port = 1, tags = Just ["x"]})
    (decodeText "host: a\nport: 1\ntags: [x]\n")
  assertEqual
    "missing field"
    (Just (1, 1, "missing key \"port\""))
    (errorOf (decodeText @Server "host: a\n"))
  assertEqual
    "optional field with a key that is not a string"
    (Just (1, 1, "the key true is a boolean, not a string"))
    (errorOf (decodeText @Switch "true: 1\n"))
  assertEqual
    "field with a default and a key that is not a string"
    (Just (1, 1, "the key true is a boolean, not a string"))
    (errorOf (decodeText @DefaultSwitch "true: 1\n"))
  assertEqual
    "key that is not a string, as the input writes it"
    (Just (1, 1, "the key True is a boolean, not a string"))
    (errorOf (decodeText @Switch "True: 1\n"))
  assertEqual
    "quoted key"
    (Right (Switch (Just 1)))
    (decodeText "'true': 1\n")
  roundTrip "round trip" Server {host = "a", port = 1, tags = Just ["x", "y"]}

test_parameters :: Assertion
test_parameters = do
  assertEqual
    "encoded"
    "left: 1\nright: 2\n"
    (encodeText (Pair @Int 1 2))
  roundTrip "round trip" (Pair @Int 1 2)
  roundTrip "round trip of lists" (Pair @[Int] [1, 2] [])
  roundTrip "round trip of nested types" (Pair (Pair @T.Text "a" "b") (Pair "c" "d"))
  assertEqual
    "missing key of an optional field"
    (Right (Pair Nothing (Just 1)))
    (decodeText @(Pair (Maybe Int)) "right: 1\n")
  assertEqual
    "missing key of a required field"
    (Just (1, 1, "missing key \"left\""))
    (errorOf (decodeText @(Pair Int) "right: 1\n"))
  assertEqual
    "path of an error"
    (Left ["right"])
    $ first
      (map (renderPath . (.path)) . NE.toList)
      (decodeText @(Pair Int) "left: 1\nright: x\n")
  assertEqual
    "null field left out"
    "name: a\n"
    (encodeText (Sparse @(Maybe Int) "a" Nothing))
  assertEqual
    "field that is not null"
    "name: a\nextra: 1\n"
    (encodeText (Sparse @(Maybe Int) "a" (Just 1)))
  roundTrip "round trip with a null field left out" (Sparse @(Maybe Int) "a" Nothing)
  assertEqual
    "null field with a comment"
    "name: a\n# b\nextra: null\n"
    . encodeText
    $ Sparse "a" (Commented (Nothing @Int) noComments {before = [Comment "b"]})
  assertEqual
    "null field without comments left out"
    "name: a\n"
    (encodeText (Sparse "a" (Commented (Nothing @Int) noComments)))
  let anchored = (S.plainNode "") {S.props = S.noProps {S.anchor = Just "x"}}
      shared =
        encodeText [Sparse "a" anchored, Sparse "b" (S.contentNode (S.AliasContent "x"))]
  assertEqual
    "null field with an anchor"
    "- name: a\n  extra: &x\n- name: b\n  extra: *x\n"
    shared
  assertEqual
    "alias to a null field read back"
    (Right [Sparse "a" Null, Sparse "b" Null])
    (decodeText @[Sparse Value] shared)
  assertEqual
    "encoded sum"
    "- tag: Filled\n  contents: 1\n- tag: Vacant\n"
    (encodeText [Filled @Int 1, Vacant])
  roundTrip "round trip of a sum" [Filled @Int 1, Vacant]

-- | A derived decoder reports the errors of all its fields.
test_collectedErrors :: Assertion
test_collectedErrors = do
  let fields = decodeText @Server "host: [a]\nport: x\ntags: [1, b, 2]\n"
  assertEqual
    "fields"
    [ (1, 7, "expected a string, but got a list")
    , (2, 7, "expected an integer, but got a string")
    , (3, 8, "expected a string, but got an integer, quote the value, e.g. '1'")
    , (3, 14, "expected a string, but got an integer, quote the value, e.g. '2'")
    ]
    (errorsOf fields)
  assertEqual
    "paths"
    (Left ["host", "port", "tags[0]", "tags[2]"])
    (first (map (renderPath . (.path)) . NE.toList) fields)
  assertEqual
    "missing and invalid fields"
    [(1, 1, "missing key \"host\""), (1, 7, "expected an integer, but got a string")]
    (errorsOf (decodeText @Server "port: x\n"))
  assertEqual
    "unknown key and invalid field"
    [ (1, 7, "expected an integer, but got a string")
    , (2, 1, "unknown key \"colour\", expected one of: size, note")
    ]
    (errorsOf (decodeText @Strict "size: x\ncolour: red\n"))
  assertEqual
    "unknown keys"
    [ (1, 1, "unknown key \"colour\", expected one of: size, note")
    , (3, 1, "unknown key \"nate\", did you mean \"note\"?")
    ]
    (errorsOf (decodeText @Strict "colour: red\nsize: 1\nnate: x\n"))
  assertEqual
    "fields of a constructor"
    [ (1, 25, "expected a number, but got a string")
    , (1, 36, "expected a number, but got a string")
    ]
    (errorsOf (decodeText @Shape "{tag: Rectangle, width: x, height: y}"))
  assertEqual
    "items of a list"
    [(1, 19, "expected an integer, but got a string"), (2, 3, "missing key \"port\"")]
    (errorsOf (decodeText @[Server] "- {host: a, port: x}\n- host: b\n"))

test_enumeration :: Assertion
test_enumeration = do
  assertEqual
    "decoded"
    (Right [TurnLeft, TurnRight])
    (decodeText "[TurnLeft, TurnRight]")
  assertEqual
    "unknown value"
    (Just (1, 1, "unknown value \"Up\", expected one of: TurnLeft, TurnRight"))
    (errorOf (decodeText @Turn "Up"))
  assertEqual
    "null"
    (Just (1, 1, "expected one of: TurnLeft, TurnRight, but got null"))
    (errorOf (decodeText @Turn "null"))
  assertEqual
    "value that needs quotes"
    (Just (1, 1, "expected a string, but got an integer, quote the value, e.g. '1'"))
    (errorOf (decodeText @Level "1"))
  assertEqual
    "quoted value"
    (Right One)
    (decodeText "'1'")
  assertEqual
    "tag of a sum"
    (Just (1, 6, "expected one of: Circle, Rectangle, Dot, but got null"))
    (errorOf (decodeText @Shape "tag: null\n"))
  assertEqual
    "misspelled value"
    (Just (1, 1, "unknown value \"TurnLetf\", did you mean \"TurnLeft\"?"))
    (errorOf (decodeText @Turn "TurnLetf"))

test_sum :: Assertion
test_sum = do
  mapM_ (\s -> roundTrip (show s) s) [Circle 1, Rectangle 2 3, Dot]
  mapM_ (\s -> roundTrip (show s) s) [Label "x", Number 1, End]
  assertEqual
    "unknown tag"
    (Just (1, 6, "unknown tag \"Square\", expected one of: Circle, Rectangle, Dot"))
    (errorOf (decodeText @Shape "tag: Square\n"))
  assertEqual
    "misspelled tag"
    (Just (1, 6, "unknown tag \"Rectangel\", did you mean \"Rectangle\"?"))
    (errorOf (decodeText @Shape "tag: Rectangel\n"))
  assertEqual
    "missing tag"
    (Just (1, 1, "missing key \"tag\""))
    (errorOf (decodeText @Shape "radius: 1\n"))

test_options :: Assertion
test_options = do
  assertEqual
    "null field left out"
    "size: 1\n"
    (encodeText (Strict 1 Nothing))
  assertEqual
    "unknown field"
    (Just (2, 1, "unknown key \"colour\", expected one of: size, note"))
    (errorOf (decodeText @Strict "size: 1\ncolour: red\n"))
  assertEqual
    "unknown field ignored"
    (Right (Loose 1))
    (decodeText "size: 1\ncolour: red\n")
  assertEqual
    "key that is not a string"
    (Just (2, 1, "expected a string as the key, but got an integer"))
    (errorOf (decodeText @Strict "size: 1\n2: x\n"))
  assertEqual
    "tag key and modifiers"
    "command: forward\nstep_count: 3\n"
    (encodeText (Forward 3))
  roundTrip "tag key and modifiers" (Forward 3)
  assertEqual
    "contents key"
    "tag: Level\nvalue: 3\n"
    (encodeText (Level 3))
  roundTrip "contents key" [Level 3, Mute]
  assertEqual
    "missing contents key"
    (Just (1, 1, "missing key \"value\""))
    (errorOf (decodeText @Volume "tag: Level\n"))
  assertEqual
    "default contents key"
    [ (1, 1, "missing key \"value\"")
    , (2, 1, "unknown key \"contents\", expected one of: tag, value")
    ]
    (errorsOf (decodeText @Volume "tag: Level\ncontents: 3\n"))
  assertEqual
    "flat contents key"
    "tag: Sent\nspeed:\n  speed: 1\n"
    (encodeText (Sent (Speed 1)))
  assertEqual
    "flat contents key without a mapping"
    "tag: Held\nspeed: 5\n"
    (encodeText (Held 5))
  assertEqual
    "flat default contents key"
    "tag: Packaged\ncontents: 1\n"
    (encodeText (Packaged (Box 1)))
  roundTrip "flat contents key" [Sent (Speed 1), Held 5, Packaged (Box 1)]

test_missingContents :: Assertion
test_missingContents = do
  assertEqual
    "field that accepts null"
    (Right (Answer Nothing))
    (decodeText "tag: Answer\n")
  assertEqual
    "field that does not accept null"
    (Just (1, 1, "missing key \"contents\""))
    (errorOf (decodeText @Token "tag: Label\n"))

-- | The errors of the single-field encoding, and the comments of its key.
test_singleField :: Assertion
test_singleField = do
  assertEqual
    "second key"
    (Just (1, 22, "expected a mapping with one key, but got a second key"))
    (errorOf (decodeText @Figure "{Round: {radius: 1}, Point: x}"))
  assertEqual
    "unknown constructor"
    (Just (1, 1, "unknown constructor \"Rund\", did you mean \"Round\"?"))
    (errorOf (decodeText @Figure "Rund: {radius: 1}"))
  assertEqual
    "constructor with fields as a string"
    ( Just
        (1, 1, "expected a mapping with the key \"Round\", because the constructor has fields")
    )
    (errorOf (decodeText @Figure "Round"))
  assertEqual
    "constructor without fields as a mapping"
    (Just (1, 1, "expected the string \"Point\", because the constructor has no fields"))
    (errorOf (decodeText @Figure "Point: x"))
  assertEqual
    "empty mapping"
    (Just (1, 1, "expected a mapping with one key, but got an empty mapping"))
    (errorOf (decodeText @Figure "{}"))
  assertEqual
    "neither a string nor a mapping"
    (Just (1, 1, "expected a string or a mapping with one key, but got an integer"))
    (errorOf (decodeText @Figure "1"))
  assertEqual
    "constructor that needs quotes"
    (Just (1, 1, "expected a string, but got a boolean, quote the value, e.g. 'false'"))
    (errorOf (decodeText @Lamp "false"))
  assertEqual
    "quoted constructor"
    (Right Dark)
    (decodeText "'false'")
  assertEqual
    "unknown field in the value"
    (Just (2, 3, "unknown key \"lvl\", did you mean \"level\"?"))
    (errorOf (decodeText @Gauge "Gauge:\n  lvl: 2\n  level: 1\n"))
  let input = "# The memo.\nMemo: hello # inline\n"
  assertEqual
    "comments of the key"
    (Right input)
    (encodeText <$> decodeText @Memo input)

test_flatten :: Assertion
test_flatten = do
  assertEqual
    "enumeration"
    "step: Rotate\ncontents: Clockwise\n"
    (encodeText (Rotate Clockwise))
  assertEqual
    "no mapping"
    "step: Wait\ncontents: 5\n"
    (encodeText (Wait 5))
  assertEqual
    "tag key"
    "step: Again\ncontents:\n  step: Halt\n"
    (encodeText (Again Halt))
  assertEqual
    "contents key"
    "step: Boxed\ncontents:\n  contents: 1\n"
    (encodeText (Boxed (Box 1)))
  assertEqual
    "contents key with other keys"
    "step: Packed\ncontents:\n  contents: 1\n  size: 2\n"
    (encodeText (Packed (Crate 1 2)))
  mapM_
    (\s -> roundTrip (show s) s)
    [ Ahead (Distance (Just 10))
    , Ahead (Distance Nothing)
    , Rotate Anticlockwise
    , Accelerate (Speed 2)
    , Halt
    , Wait 5
    , Again (Again (Rotate Clockwise))
    , Boxed (Box 1)
    , Packed (Crate 1 2)
    ]
  assertEqual
    "missing field"
    (Right (Ahead (Distance Nothing)))
    (decodeText "step: Ahead\n")
  let onlyTagNote :: String -> Int -> (Int, Int, String)
      onlyTagNote constructor column =
        ( 1
        , column
        , "the mapping has no key \"contents\" and no other keys for the field of " ++ constructor
        )
  assertEqual
    "error in a field"
    [(1, 1, "missing key \"speed\""), onlyTagNote "Accelerate" 7]
    (errorsOf (decodeText @Step "step: Accelerate\n"))
  assertEqual
    "only the tag for a field that is not a mapping"
    [(1, 1, "expected an integer, but got a mapping"), onlyTagNote "Wait" 7]
    (errorsOf (decodeText @Step "step: Wait\n"))
  assertEqual
    "misspelled field"
    [ (1, 1, "missing key \"speed\"")
    , (2, 1, "unknown key \"sped\", did you mean \"speed\"?")
    ]
    (errorsOf (decodeText @Step "step: Accelerate\nsped: 2\n"))
  assertEqual
    "misspelled contents key"
    [(1, 1, "expected an integer, but got a mapping")]
    (errorsOf (decodeText @Step "step: Wait\ncontnets: 5\n"))
  assertEqual
    "error in a field with a key close to the contents key"
    [(2, 8, "expected a string, but got a list")]
    (errorsOf (decodeText @Event "tag: Opened\ntitle: [1]\ncomments: [first]\n"))
  roundTrip "field with a key close to the contents key" (Opened (Issue "a" ["b"]))
  let entry = S.mappingNode [(S.plainNode "k", S.plainNode "v")]
      anchored = entry {S.props = S.noProps {S.anchor = Just "x"}}
      route = encodeText (Route (Shared anchored) (S.contentNode (S.AliasContent "x")))
  assertEqual
    "mapping without an anchor"
    "tag: Shared\nk: v\n"
    (encodeText (Shared entry))
  assertEqual
    "mapping with an anchor"
    "first:\n  tag: Shared\n  contents: &x\n    k: v\nagain: *x\n"
    route
  assertEqual
    "alias to a mapping with an anchor read back"
    ( Right $
        Mapping
          [
            ( String "first"
            , Mapping
                [ (String "tag", String "Shared")
                , (String "contents", Mapping [(String "k", String "v")])
                ]
            )
          , (String "again", Mapping [(String "k", String "v")])
          ]
    )
    (decodeText @Value route)
  assertEqual
    "other key next to the contents key"
    [(3, 1, "unknown key \"extra\", expected one of: step, contents")]
    (errorsOf (decodeText @Step "step: Wait\ncontents: 5\nextra: 1\n"))
  assertEqual
    "misspelled field with unknown keys ignored by the outer type only"
    [ (1, 1, "missing key \"speed\"")
    , (2, 1, "unknown key \"sped\", did you mean \"speed\"?")
    ]
    (errorsOf (decodeText @Order "tag: Hasten\nsped: 2\n"))
  assertEqual
    "misspelled contents key with unknown keys ignored"
    [(1, 1, "expected an integer, but got a mapping")]
    (errorsOf (decodeText @Order "tag: Hold\ncontnets: 5\n"))
  assertEqual
    "other key next to the contents key with unknown keys ignored"
    (Right (Hold 5))
    (decodeText "tag: Hold\ncontents: 5\nextra: 1\n")
  -- The flat field of the recursive type reads the mapping again.
  assertEqual
    "duplicate tag keys reported once"
    [ (1, 1, "missing key \"step\"")
    , onlyTagNote "Again" 7
    , (2, 4, "duplicate key \"step\"")
    , (1, 1, "the first key \"step\"")
    , (3, 4, "duplicate key \"step\"")
    , (1, 1, "the first key \"step\"")
    ]
    (errorsOf (decodeText @Step "step: Again\n!a step: Again\n!b step: Halt\n"))

test_default :: Assertion
test_default = do
  assertEqual
    "all keys missing"
    ( Right
        Settings
          { name = "app"
          , retries = 3
          , proxy = Just "proxy"
          , limits = Limits 10 20
          }
    )
    (decodeText "{}")
  assertEqual
    "some keys missing"
    ( Right
        Settings
          { name = "app"
          , retries = 5
          , proxy = Just "proxy"
          , limits = Limits 10 20
          }
    )
    (decodeText "retries: 5")
  assertEqual
    "explicit null"
    ( Right
        Settings
          { name = "app"
          , retries = 3
          , proxy = Nothing
          , limits = Limits 10 20
          }
    )
    (decodeText "proxy: null")
  assertEqual
    "default of the inner type"
    ( Right
        Settings
          { name = "app"
          , retries = 3
          , proxy = Just "proxy"
          , limits = Limits 7 2
          }
    )
    (decodeText "limits: {soft: 7}")
  assertEqual
    "constructor of the default"
    (Right (Slow 4 2))
    (decodeText "tag: Slow\nlevel: 4\n")
  assertEqual
    "other constructor"
    (Just (1, 1, "missing key \"level\""))
    (errorOf (decodeText @Mode "tag: Fast\n"))
  assertEqual
    "missing contents"
    (Right (Run 3))
    (decodeText "tag: Run\n")
  roundTrip
    "round trip"
    Settings {name = "x", retries = 1, proxy = Nothing, limits = Limits 3 4}
  assertEqual
    "null fields left out only if the default is null"
    "user: x\nproxy: null\n"
    (encodeText Profile {user = "x", proxy = Nothing, note = Nothing})
  roundTrip
    "round trip of null fields"
    Profile {user = "x", proxy = Nothing, note = Nothing}
  assertEqual
    "null fields left out only if the default has no comments"
    "user: x\nnote: null\n"
    (encodeText (Remark "x" (Commented Nothing noComments)))
  roundTrip
    "round trip of a null field without comments"
    (Remark "x" (Commented Nothing noComments))

test_requiredField :: Assertion
test_requiredField = do
  assertEqual
    "present"
    (Right Account {user = "x", shell = "/bin/sh", home = Just "/home/x"})
    (decodeText "user: x\nhome: /home/x\n")
  assertEqual
    "missing"
    (Just (1, 1, "missing key \"user\""))
    (errorOf (decodeText @Account "shell: /bin/zsh\nhome: null\n"))
  assertEqual
    "explicit null"
    (Right Account {user = "x", shell = "/bin/sh", home = Nothing})
    (decodeText "user: x\nhome: null\n")
  assertEqual
    "missing field that accepts null"
    (Just (1, 1, "missing key \"home\""))
    (errorOf (decodeText @Account "user: x"))
  assertEqual
    "null field kept"
    "user: x\nshell: /bin/sh\nhome: null\n"
    (encodeText Account {user = "x", shell = "/bin/sh", home = Nothing})
  roundTrip
    "round trip of a null field"
    Account {user = "x", shell = "/bin/sh", home = Nothing}
  roundTrip
    "round trip"
    Account {user = "x", shell = "/bin/zsh", home = Just "/home/x"}
  assertEqual
    "present contents"
    (Right (Once 2))
    (decodeText "tag: Once\ncontents: 2\n")
  assertEqual
    "missing contents"
    (Just (1, 1, "missing key \"contents\""))
    (errorOf (decodeText @Task "tag: Once\n"))
  decoded <-
    try @ErrorCall (evaluate (length (show (decodeText @Login "user: x\nshell: y\n"))))
  assertEqual
    "decoder with a strict field"
    (Left (strictError "Login"))
    (first message decoded)
  encoded <- try @ErrorCall (evaluate (T.length (encodeText (Login "x" "y"))))
  assertEqual
    "encoder with a strict field"
    (Left (strictError "Login"))
    (first message encoded)
  newtypeDecoded <-
    try @ErrorCall (evaluate (length (show (decodeText @Port "port: 1\n"))))
  assertEqual
    "decoder of a newtype"
    (Left (strictError "Port"))
    (first message newtypeDecoded)
  where
    strictError :: String -> String
    strictError name =
      "requiredField in a strict field or a newtype of the default of " ++ name

    -- The equality of 'ErrorCall' also compares the location of the call.
    message :: ErrorCall -> String
    message (ErrorCall m) = m

-- | A thread killed while the decoder checks a field of the default for
-- 'requiredField' does not break the decoder for other threads.
test_interruptedDefault :: Assertion
test_interruptedDefault = do
  done <- newEmptyMVar
  worker <-
    forkIO
      (try @SomeException (evaluate (decodeText @Gated "other: 2\n")) >>= putMVar done)
  takeMVar gateEntered
  killer <- forkIO (killThread worker)
  waitBlockedOrDone killer
  putMVar gate ()
  _ <- takeMVar done
  assertEqual
    "decode after the interrupted one"
    (Right (Gated 1 2))
    (decodeText "other: 2\n")
  where
    -- The exception is on its way once the thread that throws it waits or
    -- has thrown it.
    waitBlockedOrDone :: ThreadId -> IO ()
    waitBlockedOrDone t =
      threadStatus t >>= \case
        ThreadBlocked _ -> pure ()
        ThreadFinished -> pure ()
        _ -> yield >> waitBlockedOrDone t

test_modifiers :: Assertion
test_modifiers = do
  assertEqual
    "lower camel case"
    "source_paths"
    (snakeCase "sourcePaths")
  assertEqual
    "upper camel case"
    "source_paths"
    (snakeCase "SourcePaths")
  assertEqual
    "acronym"
    "http_server"
    (snakeCase "HTTPServer")
  assertEqual
    "acronym in the middle"
    "camel_api_case"
    (snakeCase "camelAPICase")
  assertEqual
    "kebab case"
    "source-paths"
    (kebabCase "sourcePaths")
