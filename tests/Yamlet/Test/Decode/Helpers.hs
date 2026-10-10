-- | The helpers that several decoder test modules use.
module Yamlet.Test.Decode.Helpers
  ( Config (..)
  , Size (..)
  , errorWithNote
  ) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

import Yamlet
import Yamlet.Test.Helpers

data Config = Config
  { name :: T.Text
  , paths :: [FilePath]
  , jobs :: Int
  }
  deriving stock (Eq, Show)

instance FromYaml Config where
  parseYaml = withMapping $ \o -> do
    rejectUnknownKeys ["name", "paths", "jobs"] o
    Config
      <$> parseField o "name"
      <*> parseFieldDefault o "paths" []
      <*> parseFieldDefault o "jobs" 1

-- | The line, the column and the message of the only error and of its note.
errorWithNote
  :: Either (NE.NonEmpty Error) a -> Maybe ((Int, Int, String), (Int, Int, String))
errorWithNote = \case
  Left (err NE.:| [note]) -> Just (errorPlace err, errorPlace note)
  Left errs ->
    error $
      "expected an error and a note, but got " ++ show (map (.message) (NE.toList errs))
  Right _ -> Nothing

newtype Size = Size Int
  deriving stock (Eq, Show)

instance FromYaml Size where
  parseYaml = oneOf [("small", Size 1), ("large", Size 2), ("10", Size 10)]
