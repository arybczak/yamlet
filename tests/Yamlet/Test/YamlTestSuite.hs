-- | The official YAML test suite, https://github.com/yaml/yaml-test-suite.
module Yamlet.Test.YamlTestSuite (testSuiteTests) where

import Control.Applicative
import Control.Monad
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Parser qualified as J
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
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
import Yamlet.Syntax
import Yamlet.Test.YamlTestSuite.Events

-- | The tests of the suite. The directory with the data branch of the
-- repository is in @YAML_TEST_SUITE@, or in
-- @tests/fixtures/yaml-test-suite@.
testSuiteTests :: IO TestTree
testSuiteTests = do
  dir <- fromMaybe "tests/fixtures/yaml-test-suite" <$> lookupEnv "YAML_TEST_SUITE"
  exists <- doesDirectoryExist dir
  if not exists
    then
      pure . testCase "yaml-test-suite" $
        assertFailure
          "The test suite is missing, run scripts/fetch-test-suite.sh or set YAML_TEST_SUITE"
    else do
      paths <- findCases dir
      pure . testGroup "yaml-test-suite" $
        testCase "error messages" (checkErrorMessages dir paths)
          : [testCase (makeRelative dir path) (runTest path) | path <- paths]

-- | The directories of the test cases, in order.
findCases :: FilePath -> IO [FilePath]
findCases dir = do
  -- The name and tags directories link to the tests by other names.
  entries <- L.sort . filter (`notElem` ["name", "tags"]) <$> listDirectory dir
  fmap concat . forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    hasInput <- doesFileExist (path </> "in.yaml")
    if
      | isDir && hasInput -> pure [path]
      | isDir -> findCases path
      | otherwise -> pure []

-- | The error messages for the invalid inputs match the file
-- @tests/fixtures/error-messages.txt@. The messages come from heuristics
-- that look at the input around an error, so a change in one can change
-- others. If @YAMLET_ACCEPT_ERRORS@ is set, the test writes the file
-- instead.
checkErrorMessages :: FilePath -> [FilePath] -> Assertion
checkErrorMessages root paths = do
  actual <- fmap (unlines . concat) . forM paths $ \path -> do
    (name, input, isError) <- readCase path
    if not isError
      then pure []
      else do
        let message = case parseDocumentsText input of
              Left err ->
                show err.location.line
                  ++ ":"
                  ++ show err.location.column
                  ++ ": "
                  ++ err.message
              Right _ -> "no error"
        pure ["# " ++ makeRelative root path ++ ": " ++ T.unpack name, message]
  accept <- lookupEnv "YAMLET_ACCEPT_ERRORS"
  case accept of
    Just _ -> writeFile file actual
    Nothing -> do
      expected <- readFile file
      let changes =
            [ header ++ "\n- " ++ old ++ "\n+ " ++ new
            | ((header, old), (_, new)) <- zip (entries expected) (entries actual)
            , old /= new
            ]
          preface =
            "the error messages differ from "
              ++ file
              ++ ", set YAMLET_ACCEPT_ERRORS to update it"
      when (length (entries expected) /= length (entries actual)) $
        assertFailure (preface ++ ": the number of invalid inputs changed")
      unless (null changes) $ assertFailure (preface ++ ":\n" ++ unlines changes)
  where
    file :: FilePath
    file = "tests/fixtures/error-messages.txt"

    -- The pairs of a case header and its message.
    entries :: String -> [(String, String)]
    entries s = pairs (lines s)
      where
        pairs :: [String] -> [(String, String)]
        pairs = \case
          header : message : rest -> (header, message) : pairs rest
          _ -> []

-- | The name of a case, its input and whether the input is invalid.
readCase :: FilePath -> IO (T.Text, T.Text, Bool)
readCase path = do
  name <- T.strip . T.decodeUtf8 <$> BS.readFile (path </> "===")
  input <- T.decodeUtf8 <$> BS.readFile (path </> "in.yaml")
  isError <- doesFileExist (path </> "error")
  pure (name, input, isError)

runTest :: FilePath -> Assertion
runTest path = do
  (name, input, isError) <- readCase path
  let preface = T.unpack name ++ "\n" ++ T.unpack input
  case parseDocumentsText input of
    Left err
      | isError -> pure ()
      | otherwise ->
          assertFailure $ preface ++ "\nunexpected error: " ++ prettyError "in.yaml" err
    Right docs
      | isError ->
          assertFailure $
            preface
              ++ "\nexpected an error, got:\n"
              ++ unlines (map renderEvent (toEvents docs))
      | otherwise -> do
          expected <-
            lines . T.unpack . T.decodeUtf8 <$> BS.readFile (path </> "test.event")
          assertEqual
            preface
            expected
            (map renderEvent (toEvents docs))
          let styles =
                [ ("rendered", defaultRenderOptions)
                , ("rendered in block style", defaultRenderOptions {forceBlock = True})
                ]
          forM_ styles $ \(label, options) -> do
            let out = renderSyntax options docs
            case parseDocumentsText out of
              Left err ->
                assertFailure $
                  preface
                    ++ "\n"
                    ++ label
                    ++ ":\n"
                    ++ T.unpack out
                    ++ "\nerror: "
                    ++ prettyError "out.yaml" err
              Right docs' -> do
                assertEqual
                  (preface ++ "\n" ++ label ++ ":\n" ++ T.unpack out)
                  (rendered (toEvents docs))
                  (rendered (toEvents docs'))
                assertEqual
                  (preface ++ "\n" ++ label ++ " again")
                  out
                  (renderSyntax options docs')
          hasJson <- doesFileExist (path </> "in.json")
          case Y.decodeAllText @Y.Value input of
            Left err
              -- The decoder rejects duplicate keys, which the syntax allows.
              | not hasJson && "duplicate key" `L.isPrefixOf` (NE.head err).message ->
                  pure ()
              | otherwise ->
                  assertFailure $
                    preface
                      ++ "\nunexpected error: "
                      ++ prettyError "in.yaml" (NE.head err)
            Right nodes -> do
              when hasJson $ do
                json <- BS.readFile (path </> "in.json")
                expectedValues <- case A.parseOnly jsonValues json of
                  Right vs -> pure vs
                  Left err -> assertFailure $ "invalid in.json: " ++ err
                assertEqual
                  (preface ++ "\nvalues")
                  expectedValues
                  (map toJson nodes)
              let encoded = Y.encodeAllText nodes
              case Y.decodeAllText @Y.Value encoded of
                Left err ->
                  assertFailure $
                    preface
                      ++ "\nencoded:\n"
                      ++ T.unpack encoded
                      ++ "\nerror: "
                      ++ prettyError "out.yaml" (NE.head err)
                Right nodes' ->
                  assertEqual
                    (preface ++ "\nencoded:\n" ++ T.unpack encoded)
                    nodes
                    nodes'
  where
    jsonValues :: A.Parser [J.Value]
    jsonValues = many (A.skipSpace *> J.json') <* A.skipSpace <* A.endOfInput

    -- The JSON form of a value. The keys of the mappings in the tests with
    -- JSON are strings.
    toJson :: Y.Value -> J.Value
    toJson = \case
      Y.Null -> J.Null
      Y.Bool b -> J.Bool b
      Y.Int i -> J.Number (fromInteger i)
      Y.Float (Y.Finite s) -> J.Number s
      Y.Float _ -> J.Null
      Y.String t -> J.String t
      Y.Sequence xs -> J.Array . V.fromList $ map toJson xs
      Y.Mapping kvs -> J.Object $ KM.fromList [(key k, toJson v) | (k, v) <- kvs]
      Y.Tagged _ v -> toJson v
      where
        key :: Y.Value -> K.Key
        key = \case
          Y.String t -> K.fromText t
          Y.Null -> K.fromText ""
          Y.Tagged _ v -> key v
          v -> K.fromString (show v)

    -- An event in the format of the test suite.
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
        renderProps props =
          maybe "" (\a -> " &" ++ T.unpack a) props.anchor ++ case props.tag of
            NoTag -> ""
            NonSpecificTag -> " <!>"
            Tag t -> " <" ++ T.unpack t ++ ">"

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

-- | The events that the renderer keeps. It writes a start marker for every
-- document after the first one. It can rename an anchor, so a name becomes
-- the number of the names before its first use in the document.
rendered :: [Event] -> [Event]
rendered = go M.empty
  where
    go :: M.Map T.Text Int -> [Event] -> [Event]
    go names = \case
      DocumentEnd explicit : DocumentStart _ : rest ->
        DocumentEnd explicit : DocumentStart True : go M.empty rest
      e : rest ->
        let (names', e') = numbered names (withoutStyle e)
        in e' : go names' rest
      [] -> []

    numbered :: M.Map T.Text Int -> Event -> (M.Map T.Text Int, Event)
    numbered names = \case
      SequenceStart props style -> (`SequenceStart` style) <$> numberedProps props
      MappingStart props style -> (`MappingStart` style) <$> numberedProps props
      ScalarEvent props style t -> (\p -> ScalarEvent p style t) <$> numberedProps props
      AliasEvent a -> AliasEvent <$> number a
      e -> (names, e)
      where
        numberedProps :: Props -> (M.Map T.Text Int, Props)
        numberedProps props = case props.anchor of
          Just a -> (\n -> props {anchor = Just n}) <$> number a
          Nothing -> (names, props)

        number :: T.Text -> (M.Map T.Text Int, T.Text)
        number a = case M.lookup a names of
          Just i -> (names, T.pack (show i))
          Nothing -> (M.insert a (M.size names) names, T.pack (show (M.size names)))

    -- The renderer can change the styles.
    withoutStyle :: Event -> Event
    withoutStyle = \case
      SequenceStart props _ -> SequenceStart props Block
      MappingStart props _ -> MappingStart props Block
      ScalarEvent props _ t -> ScalarEvent props Plain t
      e -> e
