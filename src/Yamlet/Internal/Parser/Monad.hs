{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | A backtracking parser over the bytes of UTF-8 encoded text.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Parser.Monad
  ( -- * Parser
    P
  , Env (..)
  , ParseError (..)
  , runParser

    -- * Combinators
  , (<|>)
  , many
  , many_
  , optional
  , optional_
  , option
  , notFollowedBy

    -- * Primitives
  , env
  , pos
  , setPos
  , furthest
  , advance
  , addTagBytes
  , peek
  , peekAt
  , failure
  , guardP
  , throwAt
  , throwUnexpected
  , withEnd
  , withHandles
  , char
  , skipWhile
  , scan
  , Scanned (..)
  , withScan

    -- * Input access
  , byteAt
  , byteBefore
  , slice
  , toOffset
  ) where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Internal qualified as T
import Data.Word
import GHC.Exts (Int (I#), Int#, isTrue#, (+#), (>#))

import Yamlet.Internal.Syntax

-- | The input of the parser.
data Env = Env
  { array :: !A.Array
  , base :: !Int
  -- ^ The index of the first byte of the input.
  , end :: !Int
  -- ^ The index past the last byte that the parser can read.
  , streamEnd :: !Int
  -- ^ The index past the last byte of the input. A document ends before it
  -- at a document marker.
  , handles :: !(M.Map T.Text T.Text)
  -- ^ The tag handles of the current document.
  }

-- | An error that no backtracking can recover from: an error at the index
-- with the message, or a failure at the index in the environment, whose
-- message the caller of the parser finds.
data ParseError
  = ParseError !Int !String
  | UnexpectedParseError !Env !Int

-- | The result of a parser: a value with the new position, a failure, or an
-- error. Both the value and the failure carry the furthest position at which
-- a parser failed, the likely location of a syntax error. The value also
-- carries the bytes that the prefixes of @%TAG@ directives added to the tags,
-- see 'addTagBytes'.
type Res# a = (# (# a, Int#, Int#, Int# #) | Int# | ParseError #)

pattern OK# :: a -> Int# -> Int# -> Int# -> Res# a
pattern OK# a p f t = (# (# a, p, f, t #) | | #)

pattern Fail# :: Int# -> Res# a
pattern Fail# f = (# | f | #)

pattern Err# :: ParseError -> Res# a
pattern Err# e = (# | | e #)

{-# COMPLETE OK#, Fail#, Err# #-}

newtype P a = P (Env -> Int# -> Int# -> Int# -> Res# a)

runP :: P a -> Env -> Int# -> Int# -> Int# -> Res# a
runP (P g) = g

instance Functor P where
  fmap f (P g) = P $ \e p fu t -> case g e p fu t of
    OK# a p' fu' t' -> OK# (f a) p' fu' t'
    Fail# fu' -> Fail# fu'
    Err# err -> Err# err

instance Applicative P where
  pure a = P $ \_ p fu t -> OK# a p fu t
  (<*>) = ap
  P g *> P h = P $ \e p fu t -> case g e p fu t of
    OK# _ p' fu' t' -> h e p' fu' t'
    Fail# fu' -> Fail# fu'
    Err# err -> Err# err

  -- The default builds the result with 'fmap' and '<*>'.
  P g <* P h = P $ \e p fu t -> case g e p fu t of
    OK# a p' fu' t' -> case h e p' fu' t' of
      OK# _ p'' fu'' t'' -> OK# a p'' fu'' t''
      Fail# fu'' -> Fail# fu''
      Err# err -> Err# err
    Fail# fu' -> Fail# fu'
    Err# err -> Err# err

instance Monad P where
  P g >>= k = P $ \e p fu t -> case g e p fu t of
    OK# a p' fu' t' -> runP (k a) e p' fu' t'
    Fail# fu' -> Fail# fu'
    Err# err -> Err# err

-- | Run a parser from the given index. Return the result, the index after it
-- and the furthest failure.
runParser :: Env -> Int -> P a -> Either ParseError (Maybe a, Int, Int)
runParser e (I# p) (P g) = case g e p p 0# of
  OK# a p' fu _ -> Right (Just a, I# p', I# fu)
  Fail# fu -> Right (Nothing, I# p, I# fu)
  Err# err -> Left err

----------------------------------------
-- Combinators

infixl 3 <|>

-- | Ordered choice. Try the second parser if the first one fails.
(<|>) :: P a -> P a -> P a
P g <|> P h = P $ \e p fu t -> case g e p fu t of
  Fail# fu' -> h e p fu' t
  r -> r

-- | Zero or more times. Stop if the parser succeeds without input.
many :: P a -> P [a]
many (P g) = P $ \e p0 fu0 t0 ->
  let go acc p fu t = case g e p fu t of
        OK# a p' fu' t'
          | isTrue# (p' ># p) -> go (a : acc) p' fu' t'
          | otherwise -> OK# (reverse acc) p fu' t
        Fail# fu' -> OK# (reverse acc) p fu' t
        Err# err -> Err# err
  in go [] p0 fu0 t0

-- | Zero or more times, discard the results.
many_ :: P a -> P ()
many_ (P g) = P $ \e p0 fu0 t0 ->
  let go p fu t = case g e p fu t of
        OK# _ p' fu' t'
          | isTrue# (p' ># p) -> go p' fu' t'
          | otherwise -> OK# () p fu' t
        Fail# fu' -> OK# () p fu' t
        Err# err -> Err# err
  in go p0 fu0 t0

optional :: P a -> P (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

optional_ :: P a -> P ()
optional_ p = void p <|> pure ()

option :: a -> P a -> P a
option a p = p <|> pure a

-- | Succeed without input if the parser fails. An error of the parser stays
-- an error, as in '<|>'.
notFollowedBy :: P a -> P ()
notFollowedBy (P g) = P $ \e p fu t -> case g e p fu t of
  OK# {} -> Fail# (furthestOf p fu)
  Fail# _ -> OK# () p fu t
  Err# err -> Err# err

-- | The furthest of a position where the parser failed and the furthest
-- failure so far.
furthestOf :: Int# -> Int# -> Int#
furthestOf p fu = if isTrue# (p ># fu) then p else fu

----------------------------------------
-- Primitives

env :: P Env
env = P $ \e p fu t -> OK# e p fu t

pos :: P Int
pos = P $ \_ p fu t -> OK# (I# p) p fu t

setPos :: Int -> P ()
setPos (I# p) = P $ \_ _ fu t -> OK# () p fu t

-- | The furthest position at which a parser failed so far.
furthest :: P Int
furthest = P $ \_ p fu t -> OK# (I# fu) p fu t

advance :: Int -> P ()
advance (I# n) = P $ \_ p fu t -> OK# () (p +# n) fu t

-- | Add the bytes that the prefix of a @%TAG@ directive adds to a tag, and
-- return the bytes that such prefixes added so far. A parser that fails
-- drops its bytes, so only the tags of the result count.
addTagBytes :: Int -> P Int
addTagBytes (I# n) = P $ \_ p fu t -> let t' = t +# n in OK# (I# t') p fu t'

-- | The byte at the current position, 0 at the end of the input.
peek :: P Word8
peek = P $ \e p fu t -> OK# (byteAt e (I# p)) p fu t

-- | The byte at the given distance from the current position.
peekAt :: Int -> P Word8
peekAt k = P $ \e p fu t -> OK# (byteAt e (I# p + k)) p fu t

failure :: P a
failure = P $ \_ p fu _ -> Fail# (furthestOf p fu)

guardP :: Bool -> P ()
guardP b = unless b failure

-- | Stop with an error at the given index.
throwAt :: Int -> String -> P a
throwAt i msg = P $ \_ _ _ _ -> Err# (ParseError i msg)

-- | Stop at the index, with an error whose message the caller of the parser
-- finds.
throwUnexpected :: Int -> P a
throwUnexpected i = P $ \e _ _ _ -> Err# (UnexpectedParseError e i)

-- | Run a parser that cannot read past the given index.
withEnd :: Int -> P a -> P a
withEnd end (P g) = P $ \e p fu t -> g e {end = end} p fu t

withHandles :: M.Map T.Text T.Text -> P a -> P a
withHandles hs (P g) = P $ \e p fu t -> g e {handles = hs} p fu t

char :: Word8 -> P ()
char w = P $ \e p fu t ->
  if byteAt e (I# p) == w
    then OK# () (p +# 1#) fu t
    else Fail# (furthestOf p fu)

skipWhile :: (Word8 -> Bool) -> P ()
skipWhile f = P $ \e p fu t ->
  let go i = if f (byteAt e i) then go (i + 1) else i
  in case go (I# p) of I# p' -> OK# () p' fu t

-- | The result of a scanning loop.
data Scanned a
  = -- | The value and the index after it.
    Done !Int !a
  | -- | The input does not match. The index is the location of the mismatch.
    NoMatch !Int
  | -- | An error at the index.
    Failed !Int !String

-- | Run a pure loop over the input from the current position.
withScan :: (Env -> Int -> Scanned a) -> P a
withScan f = P $ \e p fu t -> case f e (I# p) of
  Done (I# q) a -> OK# a q fu t
  NoMatch (I# q) -> Fail# (furthestOf q fu)
  Failed i msg -> Err# (ParseError i msg)

-- | Move to the index that the function computes from the current one.
scan :: (Env -> Int -> Int) -> P ()
scan f = P $ \e p fu t -> case f e (I# p) of I# p' -> OK# () p' fu t

----------------------------------------
-- Input access

byteAt :: Env -> Int -> Word8
byteAt e i
  | i < e.end = A.unsafeIndex e.array i
  | otherwise = 0

-- | The byte before the index, 0 at the start of the input.
byteBefore :: Env -> Int -> Word8
byteBefore e i
  | i > e.base = A.unsafeIndex e.array (i - 1)
  | otherwise = 0

slice :: Env -> Int -> Int -> T.Text
slice e i j
  | j > i = T.Text e.array i (j - i)
  -- With 'T.empty', GHC moves the content of an empty quoted key, e.g. in
  -- "'': x", to a constant, and builds the node of the key as a thunk that
  -- waits for the evaluation of 'T.empty'. A pragma on a copy of 'T.empty'
  -- does not prevent this. The heap check of the render tests finds this
  -- thunk.
  | otherwise = T.Text e.array i 0

toOffset :: Env -> Int -> Offset
toOffset e i = Offset (i - e.base)
