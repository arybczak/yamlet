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
    , testCase "one constructor without field names" test_positional
    , testCase "options" test_options
    , testCase "missing contents" test_missingContents
    , testCase "flat fields" test_flatten
    , testCase "default" test_default
    , testCase "modifiers" test_modifiers
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
  | Line Double Double
  | Label T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

newtype Name = Name T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Pair = Pair Int T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

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
  | Move Distance Speed
  | Halt
  | -- Fields that do not merge.
    Wait Int
  | Twice Distance Distance
  | Again Step
  | Boxed Box
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Step where
  yamlOptions = defaultYamlOptions {tagKey = "step", flattenFields = True}

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

data Velocity = Velocity Distance Speed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Velocity where
  yamlOptions = defaultYamlOptions {flattenFields = True}

data Coordinates = Coordinates Int Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Coordinates where
  yamlOptions = defaultYamlOptions {flattenFields = True}

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
  assertEqual "record" "tag: Circle\nradius: 1.0\n" (encodeText (Circle 1))
  assertEqual "no fields" "tag: Dot\n" (encodeText Dot)
  assertEqual "fields without names" "tag: Line\ncontents:\n- 1.0\n- 2.0\n" (encodeText (Line 1 2))
  assertEqual "one field without a name" "tag: Label\ncontents: x\n" (encodeText (Label "x"))
  mapM_ (\s -> roundTrip (show s) s) [Circle 1, Rectangle 2 3, Dot, Line 1 2, Label "x"]
  assertEqual
    "unknown tag"
    (Just (1, 6, "unknown tag \"Square\", expected one of: Circle, Rectangle, Dot, Line, Label"))
    (errorOf (decodeText @Shape "tag: Square\n"))
  assertEqual "missing tag" (Just (1, 1, "missing key \"tag\"")) (errorOf (decodeText @Shape "radius: 1\n"))
  assertEqual
    "wrong number of fields"
    (Just (2, 11, "expected a list of 2 elements, but got 1"))
    (errorOf (decodeText @Shape "tag: Line\ncontents: [1]\n"))

test_positional :: Assertion
test_positional = do
  assertEqual "one field" "x\n" (encodeText (Name "x"))
  roundTrip "one field" (Name "x")
  assertEqual "several fields" "- 1\n- a\n" (encodeText (Pair 1 "a"))
  roundTrip "several fields" (Pair 1 "a")
  assertEqual
    "wrong number of fields"
    (Just (1, 1, "expected a list of 2 elements, but got 3"))
    (errorOf (decodeText @Pair "[1, a, b]"))

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
    (errorOf (decodeText @Shape "tag: Label\n"))

test_flatten :: Assertion
test_flatten = do
  assertEqual "record" "step: Ahead\ndistance: 10\n" (encodeText (Ahead (Distance (Just 10))))
  assertEqual "enumeration" "step: Rotate\ndirection: Clockwise\n" (encodeText (Rotate Clockwise))
  assertEqual "several fields" "step: Move\ndistance: 1\nspeed: 2\n" (encodeText (Move (Distance (Just 1)) (Speed 2)))
  assertEqual "no fields" "step: Halt\n" (encodeText Halt)
  assertEqual "no mapping" "step: Wait\ncontents: 5\n" (encodeText (Wait 5))
  assertEqual
    "equal keys"
    "step: Twice\ncontents:\n- distance: 1\n- distance: 2\n"
    (encodeText (Twice (Distance (Just 1)) (Distance (Just 2))))
  assertEqual "tag key" "step: Again\ncontents:\n  step: Halt\n" (encodeText (Again Halt))
  assertEqual "contents key" "step: Boxed\ncontents:\n  contents: 1\n" (encodeText (Boxed (Box 1)))
  mapM_
    (\s -> roundTrip (show s) s)
    [ Ahead (Distance (Just 10))
    , Ahead (Distance Nothing)
    , Rotate Anticlockwise
    , Move (Distance Nothing) (Speed 2)
    , Halt
    , Wait 5
    , Twice (Distance (Just 1)) (Distance Nothing)
    , Again (Again (Rotate Clockwise))
    , Boxed (Box 1)
    ]
  assertEqual "missing field" (Right (Ahead (Distance Nothing))) (decodeText "step: Ahead\n")
  assertEqual
    "error in a field"
    (Just (1, 1, "missing key \"speed\""))
    (errorOf (decodeText @Step "step: Move\ndistance: 1\n"))
  assertEqual "untagged" "distance: 1\nspeed: 2\n" (encodeText (Velocity (Distance (Just 1)) (Speed 2)))
  roundTrip "untagged" (Velocity (Distance (Just 1)) (Speed 2))
  assertEqual "untagged without mappings" "- 1\n- 2\n" (encodeText (Coordinates 1 2))
  roundTrip "untagged without mappings" (Coordinates 1 2)

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
