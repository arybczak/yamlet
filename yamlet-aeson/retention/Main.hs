{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- Full laziness would float the input out of the action of a check into the
-- list of checks, which keeps it alive.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | The checks of yamlet's retention suite for the instances of this package.
-- They run without tasty for the same reason: in a test of tasty, a major
-- collection at times kept the input of a decode alive although no value
-- referred to it.
module Main (main) where

import Control.Exception
import Control.Monad
import Data.Aeson qualified as A
import Data.IORef
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as T
import GHC.Exts (mkWeakNoFinalizer#)
import GHC.IO
import GHC.Weak
import System.Exit
import System.IO
import System.Mem
import Yamlet

import Thunks
import Yamlet.Aeson

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

-- | A decoded value does not keep the input alive, and neither do the errors
-- of a failed decode.
checks :: [Check]
checks =
  [ Check "input without a value" $ do
      input <- evaluate (T.copy "a")
      weak <- weakArray input
      performMajorGC
      kept <- isJust <$> deRefWeak weak
      pure $ if kept then Just "the check keeps the input alive" else Nothing
  , retains @[A.Value] "Value" "- {a: b, 1: c}\n- [d, 1.5, .inf]\n- !x e"
  , retains @(ViaAeson [T.Text]) "ViaAeson" "- a\n- b"
  , retains @(ViaAeson [Endpoint]) "record" "- host: a\n  tags: [b]"
  , errorRetains @A.Value "error with a key in the message" "a: 1\n!x a: 2"
  , errorRetains @(ViaAeson [Endpoint]) "error of aeson" "- host: a\n  tags: b"
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
-- input while the value is still alive, and the thunks in the value.
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
weakArray (T.Text (TA.ByteArray arr) _ _) = IO $ \s -> case mkWeakNoFinalizer# arr () s of
  (# s', w #) -> (# s', Weak w #)

data Endpoint = Endpoint {host :: T.Text, tags :: [T.Text]}
  deriving stock (Generic)
  deriving anyclass (A.FromJSON)
