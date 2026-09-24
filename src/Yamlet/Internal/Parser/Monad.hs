{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | A backtracking parser over the bytes of UTF-8 encoded text.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Parser.Monad
  ( -- * Parser
    P(..)
  , Env(..)
  , ParseError(..)
  , runParser
  , runP

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
  , peek
  , peekAt
  , failure
  , guardP
  , throwAt
  , withEnd
  , withHandles
  , char
  , skipWhile
  , scan
  , Scanned(..)
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
import GHC.Exts

import Yamlet.Internal.Syntax

-- | The input of the parser.
data Env = Env
  { array :: !A.Array
  , base :: !Int
  -- ^ The index of the first byte of the input.
  , end :: !Int
  -- ^ The index past the last byte that the parser can read.
  , handles :: !(M.Map T.Text T.Text)
  -- ^ The tag handles of the current document.
  }

-- | An error that no backtracking can recover from.
data ParseError = ParseError !Int String
  deriving stock Show

-- | The result of a parser: a value with the new position, a failure, or an
-- error. Both the value and the failure carry the furthest position at which
-- a parser failed, the likely location of a syntax error.
type Res# a = (# (# a, Int#, Int# #) | Int# | ParseError #)

pattern OK# :: a -> Int# -> Int# -> Res# a
pattern OK# a p f = (# (# a, p, f #) | | #)

pattern Fail# :: Int# -> Res# a
pattern Fail# f = (# | f | #)

pattern Err# :: ParseError -> Res# a
pattern Err# e = (# | | e #)

{-# COMPLETE OK#, Fail#, Err# #-}

newtype P a = P (Env -> Int# -> Int# -> Res# a)

runP :: P a -> Env -> Int# -> Int# -> Res# a
runP (P g) = g
{-# INLINE runP #-}

instance Functor P where
  fmap f (P g) = P $ \e p fu -> case g e p fu of
    OK# a p' fu' -> OK# (f a) p' fu'
    Fail# fu' -> Fail# fu'
    Err# err -> Err# err
  {-# INLINE fmap #-}

instance Applicative P where
  pure a = P $ \_ p fu -> OK# a p fu
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
  P g *> P h = P $ \e p fu -> case g e p fu of
    OK# _ p' fu' -> h e p' fu'
    Fail# fu' -> Fail# fu'
    Err# err -> Err# err
  {-# INLINE (*>) #-}

instance Monad P where
  P g >>= k = P $ \e p fu -> case g e p fu of
    OK# a p' fu' -> runP (k a) e p' fu'
    Fail# fu' -> Fail# fu'
    Err# err -> Err# err
  {-# INLINE (>>=) #-}

-- | Run a parser from the given index. Return the result, the index after it
-- and the furthest failure.
runParser :: Env -> Int -> P a -> Either ParseError (Maybe a, Int, Int)
runParser e (I# p) (P g) = case g e p p of
  OK# a p' fu -> Right (Just a, I# p', I# fu)
  Fail# fu -> Right (Nothing, I# p, I# fu)
  Err# err -> Left err

----------------------------------------
-- Combinators

infixl 3 <|>

-- | Ordered choice. Try the second parser if the first one fails.
(<|>) :: P a -> P a -> P a
P g <|> P h = P $ \e p fu -> case g e p fu of
  Fail# fu' -> h e p fu'
  r -> r
{-# INLINE (<|>) #-}

-- | Zero or more times. Stop if the parser succeeds without input.
many :: P a -> P [a]
many (P g) = P $ \e p0 fu0 ->
  let go acc p fu = case g e p fu of
        OK# a p' fu'
          | isTrue# (p' ># p) -> go (a : acc) p' fu'
          | otherwise -> OK# (reverse acc) p fu'
        Fail# fu' -> OK# (reverse acc) p fu'
        Err# err -> Err# err
  in go [] p0 fu0
{-# INLINE many #-}

-- | Zero or more times, discard the results.
many_ :: P a -> P ()
many_ (P g) = P $ \e p0 fu0 ->
  let go p fu = case g e p fu of
        OK# _ p' fu'
          | isTrue# (p' ># p) -> go p' fu'
          | otherwise -> OK# () p fu'
        Fail# fu' -> OK# () p fu'
        Err# err -> Err# err
  in go p0 fu0
{-# INLINE many_ #-}

optional :: P a -> P (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# INLINE optional #-}

optional_ :: P a -> P ()
optional_ p = void p <|> pure ()
{-# INLINE optional_ #-}

option :: a -> P a -> P a
option a p = p <|> pure a
{-# INLINE option #-}

-- | Succeed without input if the parser fails.
notFollowedBy :: P a -> P ()
notFollowedBy (P g) = P $ \e p fu -> case g e p fu of
  OK# _ _ _ -> Fail# (if isTrue# (p ># fu) then p else fu)
  Fail# _ -> OK# () p fu
  Err# _ -> OK# () p fu
{-# INLINE notFollowedBy #-}

----------------------------------------
-- Primitives

env :: P Env
env = P $ \e p fu -> OK# e p fu
{-# INLINE env #-}

pos :: P Int
pos = P $ \_ p fu -> OK# (I# p) p fu
{-# INLINE pos #-}

setPos :: Int -> P ()
setPos (I# p) = P $ \_ _ fu -> OK# () p fu
{-# INLINE setPos #-}

-- | The furthest position at which a parser failed so far.
furthest :: P Int
furthest = P $ \_ p fu -> OK# (I# fu) p fu
{-# INLINE furthest #-}

advance :: Int -> P ()
advance (I# n) = P $ \_ p fu -> OK# () (p +# n) fu
{-# INLINE advance #-}

-- | The byte at the current position, 0 at the end of the input.
peek :: P Word8
peek = P $ \e p fu -> OK# (byteAt e (I# p)) p fu
{-# INLINE peek #-}

-- | The byte at the given distance from the current position.
peekAt :: Int -> P Word8
peekAt k = P $ \e p fu -> OK# (byteAt e (I# p + k)) p fu
{-# INLINE peekAt #-}

failure :: P a
failure = P $ \_ p fu -> Fail# (if isTrue# (p ># fu) then p else fu)
{-# INLINE failure #-}

guardP :: Bool -> P ()
guardP b = if b then pure () else failure
{-# INLINE guardP #-}

-- | Stop with an error at the given index.
throwAt :: Int -> String -> P a
throwAt i msg = P $ \_ _ _ -> Err# (ParseError i msg)

-- | Run a parser that cannot read past the given index.
withEnd :: Int -> P a -> P a
withEnd end (P g) = P $ \e p fu -> g e { end = end } p fu
{-# INLINE withEnd #-}

withHandles :: M.Map T.Text T.Text -> P a -> P a
withHandles hs (P g) = P $ \e p fu -> g e { handles = hs } p fu
{-# INLINE withHandles #-}

char :: Word8 -> P ()
char w = P $ \e p fu -> if byteAt e (I# p) == w
  then OK# () (p +# 1#) fu
  else Fail# (if isTrue# (p ># fu) then p else fu)
{-# INLINE char #-}

skipWhile :: (Word8 -> Bool) -> P ()
skipWhile f = P $ \e p fu ->
  let go i = if f (byteAt e i) then go (i + 1) else i
  in case go (I# p) of I# p' -> OK# () p' fu
{-# INLINE skipWhile #-}

-- | The result of a scanning loop.
data Scanned a
  = Done !Int a
  -- ^ The value and the index after it.
  | NoMatch !Int
  -- ^ The input does not match. The index is the location of the mismatch.
  | Failed !Int String
  -- ^ An error at the index.

-- | Run a pure loop over the input from the current position.
withScan :: (Env -> Int -> Scanned a) -> P a
withScan f = P $ \e p fu -> case f e (I# p) of
  Done (I# q) a -> OK# a q fu
  NoMatch (I# q) -> Fail# (if isTrue# (q ># fu) then q else fu)
  Failed i msg -> Err# (ParseError i msg)
{-# INLINE withScan #-}

-- | Move to the index that the function computes from the current one.
scan :: (Env -> Int -> Int) -> P ()
scan f = P $ \e p fu -> case f e (I# p) of I# p' -> OK# () p' fu
{-# INLINE scan #-}

----------------------------------------
-- Input access

byteAt :: Env -> Int -> Word8
byteAt e i
  | i < e.end = A.unsafeIndex e.array i
  | otherwise = 0
{-# INLINE byteAt #-}

-- | The byte before the index, 0 at the start of the input.
byteBefore :: Env -> Int -> Word8
byteBefore e i
  | i > e.base = A.unsafeIndex e.array (i - 1)
  | otherwise = 0
{-# INLINE byteBefore #-}

slice :: Env -> Int -> Int -> T.Text
slice e i j
  | j > i = T.Text e.array i (j - i)
  | otherwise = T.empty
{-# INLINE slice #-}

toOffset :: Env -> Int -> Offset
toOffset e i = Offset (i - e.base)
{-# INLINE toOffset #-}
