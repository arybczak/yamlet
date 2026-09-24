-- | The official YAML test suite, https://github.com/yaml/yaml-test-suite.
module TestSuite (testSuiteTests) where

import Control.Applicative
import Control.Monad
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Parser qualified as J
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import System.Directory
import System.Environment
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet qualified as Y
import Yamlet.Error
import Yamlet.Event
import Yamlet.Internal.Parser
import Yamlet.Syntax

-- | The tests of the suite. The directory with the data branch of the
-- repository is in @YAML_TEST_SUITE@, or in @tests/yaml-test-suite@.
testSuiteTests :: IO TestTree
testSuiteTests = do
  dir <- maybe "tests/yaml-test-suite" id <$> lookupEnv "YAML_TEST_SUITE"
  exists <- doesDirectoryExist dir
  if not exists
    then pure . testCase "yaml-test-suite" $ assertFailure $
      "The test suite is missing, run scripts/fetch-test-suite.sh or set YAML_TEST_SUITE"
    else testGroup "yaml-test-suite" <$> findTests dir dir

findTests :: FilePath -> FilePath -> IO [TestTree]
findTests root dir = do
  -- The name and tags directories link to the tests by other names.
  entries <- L.sort . filter (`notElem` ["name", "tags"]) <$> listDirectory dir
  fmap concat . forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    hasInput <- doesFileExist (path </> "in.yaml")
    if | isDir && hasInput -> pure [testCase (testName path) (runTest path)]
       | isDir -> findTests root path
       | otherwise -> pure []
  where
    testName :: FilePath -> String
    testName path = makeRelative root path

runTest :: FilePath -> Assertion
runTest path = do
  input <- T.decodeUtf8 <$> BS.readFile (path </> "in.yaml")
  isError <- doesFileExist (path </> "error")
  name <- T.strip . T.decodeUtf8 <$> BS.readFile (path </> "===")
  let preface = T.unpack name ++ "\n" ++ T.unpack input
  case parseStream input of
    Left err
      | isError -> pure ()
      | otherwise -> assertFailure $ preface ++ "\nunexpected error: " ++ prettyError "in.yaml" err
    Right docs
      | isError -> assertFailure $ preface ++ "\nexpected an error, got:\n"
          ++ unlines (map renderEvent (toEvents docs))
      | otherwise -> do
          expected <- lines . T.unpack . T.decodeUtf8 <$> BS.readFile (path </> "test.event")
          assertEqual preface expected (map renderEvent (toEvents docs))
          hasJson <- doesFileExist (path </> "in.json")
          when hasJson $ do
            json <- BS.readFile (path </> "in.json")
            expectedValues <- case A.parseOnly jsonValues json of
              Right vs -> pure vs
              Left err -> assertFailure $ "invalid in.json: " ++ err
            case Y.decodeNodes input of
              Left err -> assertFailure $ preface ++ "\nunexpected error: " ++ prettyError "in.yaml" err
              Right nodes -> assertEqual (preface ++ "\nvalues") expectedValues (map toJson nodes)
  where
    jsonValues :: A.Parser [J.Value]
    jsonValues = many (A.skipSpace *> J.json') <* A.skipSpace <* A.endOfInput

-- | The JSON value of a node. The keys of the mappings in the tests with JSON
-- are strings.
toJson :: Y.Node -> J.Value
toJson n = case n.value of
  Y.Null -> J.Null
  Y.Bool b -> J.Bool b
  Y.Int i -> J.Number (fromInteger i)
  Y.Float d -> J.Number (Sci.fromFloatDigits d)
  Y.String t -> J.String t
  Y.Sequence xs -> J.Array . V.fromList $ map toJson xs
  Y.Mapping kvs -> J.Object $ KM.fromList [ (key k, toJson v) | (k, v) <- kvs ]
  where
    key :: Y.Node -> K.Key
    key k = case k.value of
      Y.String t -> K.fromText t
      Y.Null -> K.fromText ""
      v -> K.fromString (show v)

-- | Render an event in the format of the test suite.
renderEvent :: Event -> String
renderEvent = \case
  StreamStart -> "+STR"
  StreamEnd -> "-STR"
  DocumentStart explicit -> "+DOC" ++ if explicit then " ---" else ""
  DocumentEnd explicit -> "-DOC" ++ if explicit then " ..." else ""
  SequenceStart props style -> "+SEQ" ++ flow style "[]" ++ renderProps props
  SequenceEnd -> "-SEQ"
  MappingStart props style -> "+MAP" ++ flow style "{}" ++ renderProps props
  MappingEnd -> "-MAP"
  ScalarEvent props style t ->
    "=VAL" ++ renderProps props ++ " " ++ styleChar style : escape (T.unpack t)
  AliasEvent name -> "=ALI *" ++ T.unpack name
  where
    flow :: CollectionStyle -> String -> String
    flow style s = case style of
      Flow -> ' ' : s
      Block -> ""

    renderProps :: Props -> String
    renderProps props = concat
      [ maybe "" (\a -> " &" ++ T.unpack a) props.anchor
      , case props.tag of
          NoTag -> ""
          NonSpecificTag -> " <!>"
          Tag t -> " <" ++ T.unpack t ++ ">"
      ]

    styleChar :: ScalarStyle -> Char
    styleChar = \case
      Plain -> ':'
      SingleQuoted -> '\''
      DoubleQuoted -> '"'
      Literal -> '|'
      Folded -> '>'

    escape :: String -> String
    escape = concatMap $ \case
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\r' -> "\\r"
      c -> [c]
