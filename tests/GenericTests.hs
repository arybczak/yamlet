module GenericTests (genericTests) where

import Data.Aeson qualified as A
import Data.Char
import Data.Text qualified as T
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet

genericTests :: TestTree
genericTests =
  testGroup
    "Generic"
    [ testCase "record" test_record
    , testCase "enumeration" test_enumeration
    , testCase "sum" test_sum
    , shapes
    , testCase "options" test_options
    , testCase "missing contents" test_missingContents
    , testCase "flat fields" test_flatten
    , testCase "default" test_default
    , testCase "modifiers" test_modifiers
    , testCase "commented fields" test_commentedFields
    , testCase "commented values" test_commentedValues
    , testProperty "snakeCase is camelTo2 of aeson" $ forAll name $ \s -> snakeCase s === A.camelTo2 '_' s
    , testProperty "kebabCase is camelTo2 of aeson" $ forAll name $ \s -> kebabCase s === A.camelTo2 '-' s
    ]
  where
    -- Mostly letters in both cases, where the rules matter.
    name :: Gen String
    name = listOf (elements "aAbBzZ1_-")

data Server = Server {host :: T.Text, port :: Int, tags :: Maybe [T.Text]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Turn = TurnLeft | TurnRight
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Shape
  = Circle {radius :: Double}
  | Rectangle {width :: Double, height :: Double}
  | Dot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Token = Label T.Text | Number Int | End
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

newtype Name = Name T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

-- The types of the table of shapes that no other test uses.

data Unit = Unit
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data UnitMapping = UnitMapping
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml UnitMapping where
  yamlOptions = defaultYamlOptions {allNullaryToStringTag = False}

data UnitTagged = UnitTagged
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml UnitTagged where
  yamlOptions = defaultYamlOptions {allNullaryToStringTag = False, tagSingleConstructors = True}

newtype NameTagged = NameTagged T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml NameTagged where
  yamlOptions = defaultYamlOptions {tagSingleConstructors = True}

data Literal = Whole Int | Words T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Motion = Go Distance | Hurry Speed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Motion where
  type FlattenFields Motion = True

data Strict = Strict {size :: Int, note :: Maybe T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Strict where
  yamlOptions = defaultYamlOptions {rejectUnknownFields = True, omitNullFields = True}

data Command = Forward {stepCount :: Int} | Stop
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Command where
  yamlOptions =
    defaultYamlOptions
      { tagKey = "command"
      , constructorTagModifier = map toLower
      , fieldLabelModifier = concatMap (\c -> if isUpper c then ['_', toLower c] else [c])
      }

newtype Single = Single {value :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Single where
  yamlOptions = defaultYamlOptions {tagSingleConstructors = True}

data Reply = Answer (Maybe Int) | Silence
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Step where
  type FlattenFields Step = True
  yamlOptions = defaultYamlOptions {tagKey = "step"}

newtype Distance = Distance {distance :: Maybe Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

newtype Speed = Speed {speed :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

newtype Box = Box {contents :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Direction = Clockwise | Anticlockwise
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Direction where
  yamlOptions = defaultYamlOptions {tagKey = "direction", allNullaryToStringTag = False}

data Settings = Settings {name :: T.Text, retries :: Int, proxy :: Maybe T.Text, limits :: Limits}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Settings where
  yamlDefault = Just (Settings "app" 3 (Just "proxy") (Limits 10 20))

data Limits = Limits {soft :: Int, hard :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Limits where
  yamlDefault = Just (Limits 1 2)

data Mode = Fast {level :: Int} | Slow {level :: Int, delay :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Mode where
  yamlDefault = Just (Slow 1 2)

data Job = Run Int | Skip
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Job where
  yamlDefault = Just (Run 3)

data Profile = Profile {user :: T.Text, proxy :: Maybe T.Text, note :: Maybe T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Profile where
  yamlOptions = defaultYamlOptions {omitNullFields = True}
  yamlDefault = Just (Profile "app" (Just "proxy") Nothing)

-- | Records that keep the comments of their keys.
data Pipeline = Pipeline {name :: Commented T.Text, lint :: Commented Lint}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Lint = Lint {version :: Commented T.Text, level :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

-- | The comment at the top and the comment above a first key belong to the
-- mapping in the syntax tree, but the decoder gives them to the first key.
test_commentedFields :: Assertion
test_commentedFields =
  assertEqual "round trip" (Right input) (encodeText <$> decodeText @Pipeline input)
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
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

newtype Hooks = Hooks {afterSetup :: Commented [Script]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Hooks where
  yamlOptions = defaultYamlOptions {fieldLabelModifier = kebabCase}

newtype Script = Script {run :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Optional = Optional {first :: T.Text, extra :: Maybe (Commented Node)}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Note = Note (Commented T.Text) | Blank
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

-- | The comments at the end of a collection and after a value.
test_commentedValues :: Assertion
test_commentedValues = do
  assertEqual "round trip" (Right input) (encodeText <$> decodeText @Setup input)
  let optional = T.unlines ["first: a", "# The extra part.", "extra: # optional", "  x: 1"]
  assertEqual "optional field" (Right optional) (encodeText <$> decodeText @Optional optional)
  let note = T.unlines ["tag: Note", "# The text.", "contents: hello # c"]
  assertEqual "contents key" (Right note) (encodeText <$> decodeText @Note note)
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

-- | Each supported shape of constructors, with the options that change its
-- encoding.
shapes :: TestTree
shapes =
  testGroup
    "shapes"
    [ shape "one constructor without fields" Unit "Unit\n"
    , shape "one constructor without fields as a mapping" UnitMapping "{}\n"
    , shape "one constructor without fields with a tag" UnitTagged "tag: UnitTagged\n"
    , shape "one field without a name" (Name "x") "x\n"
    , shape "one field without a name with a tag" (NameTagged "x") "tag: NameTagged\ncontents: x\n"
    , shape "one named field" (Speed 1) "speed: 1\n"
    , shape "named fields" (Server "a" 1 Nothing) "host: a\nport: 1\ntags: null\n"
    , shape "named fields with a tag" (Single 1) "tag: Single\nvalue: 1\n"
    , shape "enumeration" TurnLeft "TurnLeft\n"
    , shape "enumeration as a mapping" Clockwise "direction: Clockwise\n"
    , shape "fields without names" (Whole 1) "tag: Whole\ncontents: 1\n"
    , shape "fields without names, with fields" (Label "x") "tag: Label\ncontents: x\n"
    , shape "fields without names, without fields" End "tag: End\n"
    , shape "flat fields" (Go (Distance (Just 1))) "tag: Go\ndistance: 1\n"
    , shape "flat fields, with fields" (Ahead (Distance (Just 10))) "step: Ahead\ndistance: 10\n"
    , shape "flat fields, without fields" Halt "step: Halt\n"
    , shape "named fields in a sum, one field" (Fast 1) "tag: Fast\nlevel: 1\n"
    , shape "named fields in a sum, several fields" (Slow 1 2) "tag: Slow\nlevel: 1\ndelay: 2\n"
    , shape "named fields in a sum, with fields" (Rectangle 2 3) "tag: Rectangle\nwidth: 2.0\nheight: 3.0\n"
    , shape "named fields in a sum, without fields" Dot "tag: Dot\n"
    ]
  where
    shape :: (Eq a, Show a, FromYaml a, ToYaml a) => String -> a -> T.Text -> TestTree
    shape preface x yaml = testCase preface $ do
      assertEqual "encoded" yaml (encodeText x)
      assertEqual "decoded" (Right x) (decodeText yaml)

test_record :: Assertion
test_record = do
  assertEqual "optional field" (Right (Server "a" 1 Nothing)) (decodeText "host: a\nport: 1\n")
  assertEqual "all fields" (Right (Server "a" 1 (Just ["x"]))) (decodeText "host: a\nport: 1\ntags: [x]\n")
  assertEqual "missing field" (Just (1, 1, "missing key \"port\"")) (errorOf (decodeText @Server "host: a\n"))
  assertEqual "encoded" "host: a\nport: 1\ntags: null\n" (encodeText (Server "a" 1 Nothing))
  roundTrip "round trip" (Server "a" 1 (Just ["x", "y"]))

test_enumeration :: Assertion
test_enumeration = do
  assertEqual "decoded" (Right [TurnLeft, TurnRight]) (decodeText "[TurnLeft, TurnRight]")
  assertEqual "encoded" "TurnLeft\n" (encodeText TurnLeft)
  assertEqual
    "unknown value"
    (Just (1, 1, "unknown value \"Up\", expected one of: TurnLeft, TurnRight"))
    (errorOf (decodeText @Turn "Up"))

test_sum :: Assertion
test_sum = do
  mapM_ (\s -> roundTrip (show s) s) [Circle 1, Rectangle 2 3, Dot]
  mapM_ (\s -> roundTrip (show s) s) [Label "x", Number 1, End]
  assertEqual
    "unknown tag"
    (Just (1, 6, "unknown tag \"Square\", expected one of: Circle, Rectangle, Dot"))
    (errorOf (decodeText @Shape "tag: Square\n"))
  assertEqual "missing tag" (Just (1, 1, "missing key \"tag\"")) (errorOf (decodeText @Shape "radius: 1\n"))

test_options :: Assertion
test_options = do
  assertEqual "null field left out" "size: 1\n" (encodeText (Strict 1 Nothing))
  assertEqual
    "unknown field"
    (Just (2, 1, "unknown key \"colour\", expected one of: size, note"))
    (errorOf (decodeText @Strict "size: 1\ncolour: red\n"))
  assertEqual "tag key and modifiers" "command: forward\nstep_count: 3\n" (encodeText (Forward 3))
  roundTrip "tag key and modifiers" (Forward 3)
  assertEqual "tag of one constructor" "tag: Single\nvalue: 1\n" (encodeText (Single 1))
  roundTrip "tag of one constructor" (Single 1)

test_missingContents :: Assertion
test_missingContents = do
  assertEqual "field that accepts null" (Right (Answer Nothing)) (decodeText "tag: Answer\n")
  assertEqual
    "field that does not accept null"
    (Just (1, 1, "missing key \"contents\""))
    (errorOf (decodeText @Token "tag: Label\n"))

test_flatten :: Assertion
test_flatten = do
  assertEqual "record" "step: Ahead\ndistance: 10\n" (encodeText (Ahead (Distance (Just 10))))
  assertEqual "enumeration" "step: Rotate\ndirection: Clockwise\n" (encodeText (Rotate Clockwise))
  assertEqual "no fields" "step: Halt\n" (encodeText Halt)
  assertEqual "no mapping" "step: Wait\ncontents: 5\n" (encodeText (Wait 5))
  assertEqual "tag key" "step: Again\ncontents:\n  step: Halt\n" (encodeText (Again Halt))
  assertEqual "contents key" "step: Boxed\ncontents:\n  contents: 1\n" (encodeText (Boxed (Box 1)))
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
    ]
  assertEqual "missing field" (Right (Ahead (Distance Nothing))) (decodeText "step: Ahead\n")
  assertEqual
    "error in a field"
    (Just (1, 1, "missing key \"speed\""))
    (errorOf (decodeText @Step "step: Accelerate\n"))

test_default :: Assertion
test_default = do
  assertEqual "all keys missing" (Right (Settings "app" 3 (Just "proxy") (Limits 10 20))) (decodeText "{}")
  assertEqual "some keys missing" (Right (Settings "app" 5 (Just "proxy") (Limits 10 20))) (decodeText "retries: 5")
  assertEqual "explicit null" (Right (Settings "app" 3 Nothing (Limits 10 20))) (decodeText "proxy: null")
  assertEqual "default of the inner type" (Right (Settings "app" 3 (Just "proxy") (Limits 7 2))) (decodeText "limits: {soft: 7}")
  assertEqual "constructor of the default" (Right (Slow 4 2)) (decodeText "tag: Slow\nlevel: 4\n")
  assertEqual
    "other constructor"
    (Just (1, 1, "missing key \"level\""))
    (errorOf (decodeText @Mode "tag: Fast\n"))
  assertEqual "missing contents" (Right (Run 3)) (decodeText "tag: Run\n")
  roundTrip "round trip" (Settings "x" 1 Nothing (Limits 3 4))
  assertEqual
    "null fields left out only if the default is null"
    "user: x\nproxy: null\n"
    (encodeText (Profile "x" Nothing Nothing))
  roundTrip "round trip of null fields" (Profile "x" Nothing Nothing)

test_modifiers :: Assertion
test_modifiers = do
  assertEqual "lower camel case" "source_paths" (snakeCase "sourcePaths")
  assertEqual "upper camel case" "source_paths" (snakeCase "SourcePaths")
  assertEqual "acronym" "http_server" (snakeCase "HTTPServer")
  assertEqual "acronym in the middle" "camel_api_case" (snakeCase "camelAPICase")
  assertEqual "kebab case" "source-paths" (kebabCase "sourcePaths")

-- | Encoding a value and decoding the result gives the same value.
roundTrip :: (Eq a, Show a, ToYaml a, FromYaml a) => String -> a -> Assertion
roundTrip preface x = assertEqual preface (Right x) (decodeText (encodeText x))

errorOf :: Either Error a -> Maybe (Int, Int, String)
errorOf = \case
  Left err -> Just (err.location.line, err.location.column, err.message)
  Right _ -> Nothing
