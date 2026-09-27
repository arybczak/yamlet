{-# OPTIONS_HADDOCK not-home #-}

-- | The checks of a syntax tree before the decoder reads it, and the values
-- of its nodes.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Compose
  ( prepare
  , represent
  , Failure
  ) where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T

import Yamlet.Internal.Schema
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Internal.Utils
import Yamlet.Value

-- | Check that the tags of a node are valid and that the keys of every
-- mapping are unique, and replace each alias with the node that it refers
-- to. The result has no aliases. A node without aliases comes back
-- unchanged.
prepare :: S.Node -> Either Failure S.Node
prepare root
  | needsNumbering root = expandAliases root <$ represent root
  | otherwise = root <$ check root

-- | The value of a node, with the checks of 'prepare'.
represent :: S.Node -> Either Failure Value
represent root
  | needsNumbering root = fst . fst <$> go (Numbering M.empty M.empty 0) root
  | otherwise = plain root
  where
    -- The limit of the visits of a traversal of the document. Aliases can add
    -- as many visits as the document has nodes, or 'smallBudget' for a small
    -- document. Without a limit, the visits of a small input can be
    -- exponential in its size.
    limit :: Int
    limit = n + max smallBudget n
      where
        n :: Int
        n = syntaxSize root

        -- A traversal of 100000 nodes takes about 5 ms and 6 MB, measured
        -- with a copy of the nodes. go-yaml allows about 400000 nodes from
        -- aliases in a small document.
        smallBudget :: Int
        smallBudget = 100000

        syntaxSize :: S.Node -> Int
        syntaxSize sn = case sn.content of
          S.Sequence _ xs -> 1 + sum (map syntaxSize xs)
          S.Mapping _ kvs -> 1 + sum [syntaxSize k + syntaxSize v | (k, v) <- kvs]
          _ -> 1

    -- Without aliases the anchors do not matter, and without collection keys
    -- only scalar keys compare.
    plain :: S.Node -> Either Failure Value
    plain sn =
      let off = sn.offset; props = sn.props
      in case sn.content of
           S.Scalar style t -> scalar off props style t
           S.Sequence _ xs -> do
             tag <- collectionTag off props seqTag
             vs <- mapM plain xs
             Right $ withTag tag (Sequence vs)
           S.Mapping _ kvs -> do
             tag <- collectionTag off props mapTag
             entries <- mapM (\(k, v) -> (,) <$> plain k <*> plain v) kvs
             checkUniqueKeys (zip (map ((.offset) . fst) kvs) (map fst entries))
             Right $ withTag tag (Mapping entries)
           S.Alias _ -> Left $ failure off "unexpected alias"

    -- Each value comes with its number.
    go :: Numbering -> S.Node -> Either Failure ((Value, Int), Numbering)
    go st sn =
      let off = sn.offset; props = sn.props
      in case sn.content of
           S.Alias name -> case M.lookup name st.anchors of
             Just (Just (v, i, visits))
               | st.visits + visits > limit ->
                   Left
                     $ failure off
                     $ "the aliases expand the document to more than " ++ show limit ++ " nodes"
               | otherwise -> Right ((v, i), st {visits = st.visits + visits})
             Just Nothing ->
               Left
                 $ failure off
                 $ "the alias *" ++ T.unpack name ++ " refers to a node that contains it"
             Nothing ->
               Left
                 $ failure off
                 $ "undefined alias *" ++ T.unpack name
           S.Scalar style t -> do
             v <- scalar off props style t
             Right $ number props v (ScalarShape v) 1 st
           S.Sequence _ xs -> do
             tag <- collectionTag off props seqTag
             (vs, st') <- goList (open props st) xs
             let v = withTag tag (Sequence (map fst vs))
             Right $ number props v (SequenceShape tag (map snd vs)) (st'.visits - st.visits + 1) st'
           S.Mapping _ kvs -> do
             tag <- collectionTag off props mapTag
             (entries, st') <- goPairs (open props st) kvs
             checkUniqueNumbers entries
             let v = withTag tag (Mapping [(k, x) | (_, (k, _), (x, _)) <- entries])
                 shape = MappingShape tag (L.sort [(i, j) | (_, (_, i), (_, j)) <- entries])
             Right $ number props v shape (st'.visits - st.visits + 1) st'

    goList :: Numbering -> [S.Node] -> Either Failure ([(Value, Int)], Numbering)
    goList st = \case
      [] -> Right ([], st)
      x : xs -> do
        (v, st') <- go st x
        (vs, st'') <- goList st' xs
        Right (v : vs, st'')

    -- The entries come with the offsets of their keys.
    goPairs
      :: Numbering
      -> [(S.Node, S.Node)]
      -> Either Failure ([(S.Offset, (Value, Int), (Value, Int))], Numbering)
    goPairs st = \case
      [] -> Right ([], st)
      (k, v) : kvs -> do
        (kv, st') <- go st k
        (vv, st'') <- go st' v
        (rest, st''') <- goPairs st'' kvs
        Right ((k.offset, kv, vv) : rest, st''')

    open :: S.Props -> Numbering -> Numbering
    open props st = case props.anchor of
      Just a -> st {anchors = M.insert a Nothing st.anchors}
      Nothing -> st

    -- Give the value the number of its shape, and define its anchor. The
    -- visits are those of the node and of everything inside it.
    number :: S.Props -> Value -> Shape -> Int -> Numbering -> ((Value, Int), Numbering)
    number props v shape visits st = ((v, i), Numbering anchors' shapes' (st.visits + 1))
      where
        i :: Int
        shapes' :: M.Map Shape Int
        (i, shapes') = case M.lookup shape st.shapes of
          Just j -> (j, st.shapes)
          Nothing -> let j = M.size st.shapes in (j, M.insert shape j st.shapes)

        anchors' :: M.Map T.Text (Maybe (Value, Int, Int))
        anchors' = case props.anchor of
          Just a -> M.insert a (Just (v, i, visits)) st.anchors
          Nothing -> st.anchors

    -- Unlike in 'duplicate', comparing all pairs is not faster for few keys.
    checkUniqueNumbers :: [(S.Offset, (Value, Int), (Value, Int))] -> Either Failure ()
    checkUniqueNumbers = loop IS.empty
      where
        loop :: IS.IntSet -> [(S.Offset, (Value, Int), (Value, Int))] -> Either Failure ()
        loop seen = \case
          [] -> Right ()
          (off, (k, i), _) : rest
            | i `IS.member` seen -> Left $ duplicateKey (off, k)
            | otherwise -> loop (IS.insert i seen) rest

-- | The offset of the node that caused an error, and the message.
type Failure = (S.Offset, String)

failure :: S.Offset -> String -> Failure
failure = (,)

-- | The checks of 'represent' for a node without aliases and collection keys.
-- Only the keys get values, for the comparison.
check :: S.Node -> Either Failure ()
check sn =
  let off = sn.offset; props = sn.props
  in case sn.content of
       S.Scalar style t
         -- Only a number can fail without a tag.
         | S.NoTag <- props.tag
         , style /= S.Plain || not (maybeNumber t) ->
             Right ()
         | otherwise -> void (scalar off props style t)
       S.Sequence _ xs -> collectionTag off props seqTag *> traverse_ check xs
       S.Mapping _ kvs -> do
         _ <- collectionTag off props mapTag
         keys <- traverse (\(k, v) -> key k <* check v) kvs
         checkUniqueKeys keys
       S.Alias _ -> Left $ failure off "unexpected alias"
  where
    maybeNumber :: T.Text -> Bool
    maybeNumber t = case T.uncons t of
      Just (c, _) -> isDigit c || c == '-' || c == '+' || c == '.'
      Nothing -> False

    key :: S.Node -> Either Failure (S.Offset, Value)
    key k = case k.content of
      S.Scalar style t -> (k.offset,) <$> scalar k.offset k.props style t
      _ -> Left $ failure k.offset "unexpected collection key"

-- | Replace each alias with a copy of the node that it refers to. The copy
-- has the offsets and the comments of the alias, and no anchor. The node
-- must pass 'represent', so every alias refers to an earlier anchor.
expandAliases :: S.Node -> S.Node
expandAliases = fst . go M.empty
  where
    go :: M.Map T.Text S.Node -> S.Node -> (S.Node, M.Map T.Text S.Node)
    go anchors sn = case sn.content of
      S.Alias name -> case M.lookup name anchors of
        Just target ->
          ( S.Node sn.offset sn.endOffset (S.Props Nothing target.props.tag) sn.comments target.content
          , anchors
          )
        Nothing -> (sn, anchors)
      S.Scalar {} -> define sn anchors
      S.Sequence style xs ->
        let (xs', anchors') = goList anchors xs
        in define (withContent (S.Sequence style xs')) anchors'
      S.Mapping style kvs ->
        let (kvs', anchors') = goPairs anchors kvs
        in define (withContent (S.Mapping style kvs')) anchors'
      where
        withContent :: S.Content -> S.Node
        withContent = S.Node sn.offset sn.endOffset sn.props sn.comments

    define :: S.Node -> M.Map T.Text S.Node -> (S.Node, M.Map T.Text S.Node)
    define sn anchors = case sn.props.anchor of
      Just a -> (sn, M.insert a sn anchors)
      Nothing -> (sn, anchors)

    goList :: M.Map T.Text S.Node -> [S.Node] -> ([S.Node], M.Map T.Text S.Node)
    goList anchors = \case
      [] -> ([], anchors)
      x : xs ->
        let (x', anchors') = go anchors x
            (xs', anchors'') = goList anchors' xs
        in (x' : xs', anchors'')

    goPairs :: M.Map T.Text S.Node -> [(S.Node, S.Node)] -> ([(S.Node, S.Node)], M.Map T.Text S.Node)
    goPairs anchors = \case
      [] -> ([], anchors)
      (k, v) : kvs ->
        let (k', anchors') = go anchors k
            (v', anchors'') = go anchors' v
            (kvs', anchors''') = goPairs anchors'' kvs
        in ((k', v') : kvs', anchors''')

-- | The value with the tag, in 'Tagged' if the tag is not the one of the core
-- schema for the value.
withTag :: T.Text -> Value -> Value
withTag tag v
  | tag == valueTag v = v
  | otherwise = Tagged tag v

scalar :: S.Offset -> S.Props -> S.ScalarStyle -> T.Text -> Either Failure Value
scalar off props style t = case props.tag of
  S.NoTag
    | style == S.Plain -> case resolvePlainExact t of
        Right v -> Right v
        Left _ -> Left $ failure off exponentOutOfRange
    | otherwise -> Right (String t)
  S.NonSpecificTag -> Right (String t)
  S.Tag tag
    | tag == seqTag || tag == mapTag ->
        Left
          $ failure off
          $ "the tag !!" ++ T.unpack (T.drop (T.length coreTagPrefix) tag) ++ " cannot be used on a scalar"
    | otherwise -> case resolveTaggedExact tag t of
        Just (Right v) -> Right (withTag tag v)
        Just (Left _) -> Left $ failure off exponentOutOfRange
        Nothing ->
          Left
            $ failure off
            $ "invalid value for the tag !!"
              ++ T.unpack (T.drop (T.length coreTagPrefix) tag)
              ++ if tag == boolTag && isYaml11Bool t
                then ", " ++ show t ++ " is a boolean only in YAML 1.1"
                else ""

collectionTag :: S.Offset -> S.Props -> T.Text -> Either Failure T.Text
collectionTag off props def = case props.tag of
  S.NoTag -> Right def
  S.NonSpecificTag -> Right def
  S.Tag tag
    | tag == def || not (isCoreTag tag) -> Right tag
    | otherwise ->
        Left
          $ failure off
          $ "the tag !!"
            ++ T.unpack (T.drop (T.length coreTagPrefix) tag)
            ++ " cannot be used on a "
            ++ (if def == seqTag then "sequence" else "mapping")

isCoreTag :: T.Text -> Bool
isCoreTag tag = tag `elem` [nullTag, boolTag, intTag, floatTag, strTag, seqTag, mapTag]

-- | The keys come with their offsets.
checkUniqueKeys :: [(S.Offset, Value)] -> Either Failure ()
checkUniqueKeys keys = case duplicate keys of
  Just k -> Left $ duplicateKey k
  Nothing -> Right ()

duplicateKey :: (S.Offset, Value) -> Failure
duplicateKey (off, k) = failure off $ case k of
  -- YAML 1.1 used "<<" to merge mappings, and some tools still do.
  String "<<" -> "duplicate key \"<<\", merge keys are not supported"
  String t -> "duplicate key " ++ show t
  _ -> "duplicate key"

-- | The state of the composition of a document with aliases or collection
-- keys. Equal values get the same number, so that keys compare in constant
-- time, even if they are large collections or come from aliases that expand
-- to huge values.
data Numbering = Numbering
  { anchors :: !(M.Map T.Text (Maybe (Value, Int, Int)))
  -- ^ An anchor maps to its value, the number of the value and the visits of
  -- a traversal of the node. It maps to Nothing while its node is composed.
  , shapes :: !(M.Map Shape Int)
  , visits :: !Int
  -- ^ The visits of a traversal of the nodes so far.
  }

-- | A value with the numbers of its items or entries in place of them. The
-- entries of a mapping are sorted, so their order does not matter. A scalar
-- is small, so it stays a value.
data Shape
  = ScalarShape !Value
  | SequenceShape !T.Text [Int]
  | MappingShape !T.Text [(Int, Int)]
  deriving stock (Eq, Ord)

-- | The node has an alias or a collection key inside it.
needsNumbering :: S.Node -> Bool
needsNumbering n = case n.content of
  S.Scalar {} -> False
  S.Sequence _ xs -> any needsNumbering xs
  S.Mapping _ kvs -> any (\(k, v) -> isCollection k || needsNumbering k || needsNumbering v) kvs
  S.Alias {} -> True
  where
    isCollection :: S.Node -> Bool
    isCollection k = case k.content of
      S.Sequence {} -> True
      S.Mapping {} -> True
      _ -> False

-- | The first scalar key that is equal to an earlier one. A document with a
-- collection key gets numbers for its keys instead.
duplicate :: [(S.Offset, Value)] -> Maybe (S.Offset, Value)
duplicate keys = case drop maxPairwise keys of
  _ : _ -> viaSet Set.empty keys
  [] -> pairwise [] keys
  where
    -- Comparing all pairs is faster for 16 keys or fewer, by a measurement.
    maxPairwise :: Int
    maxPairwise = 16

    viaSet :: Set.Set Value -> [(S.Offset, Value)] -> Maybe (S.Offset, Value)
    viaSet seen = \case
      [] -> Nothing
      k@(_, v) : ks
        | v `Set.member` seen -> Just k
        | otherwise -> viaSet (Set.insert v seen) ks

    pairwise :: [Value] -> [(S.Offset, Value)] -> Maybe (S.Offset, Value)
    pairwise seen = \case
      [] -> Nothing
      k@(_, v) : ks
        | v `elem` seen -> Just k
        | otherwise -> pairwise (v : seen) ks
