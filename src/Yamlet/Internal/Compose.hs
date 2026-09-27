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
  , noMergeKeys
  ) where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
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
             checkUniqueKeys (zip (map fst kvs) (map fst entries))
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

    -- The entries come with the nodes of their keys.
    goPairs
      :: Numbering
      -> [(S.Node, S.Node)]
      -> Either Failure ([(S.Node, (Value, Int), (Value, Int))], Numbering)
    goPairs st = \case
      [] -> Right ([], st)
      (k, v) : kvs -> do
        (kv, st') <- go st k
        (vv, st'') <- go st' v
        (rest, st''') <- goPairs st'' kvs
        Right ((k, kv, vv) : rest, st''')

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
    checkUniqueNumbers :: [(S.Node, (Value, Int), (Value, Int))] -> Either Failure ()
    checkUniqueNumbers = loop IM.empty
      where
        loop :: IM.IntMap (S.Node, Value) -> [(S.Node, (Value, Int), (Value, Int))] -> Either Failure ()
        loop seen = \case
          [] -> Right ()
          (kn, (k, i), _) : rest -> case IM.lookup i seen of
            Just first -> Left $ duplicateKey (kn, k) first
            Nothing -> loop (IM.insert i (kn, k) seen) rest

-- | The offset of the node that caused an error and the message, and the
-- notes that go after it, e.g. the first key of a duplicate key.
type Failure = NE.NonEmpty (S.Offset, String)

failure :: S.Offset -> String -> Failure
failure off msg = (off, msg) NE.:| []

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

    key :: S.Node -> Either Failure (S.Node, Value)
    key k = case k.content of
      S.Scalar style t -> (k,) <$> scalar k.offset k.props style t
      _ -> Left $ failure k.offset "unexpected collection key"

-- | Replace each alias with a copy of the node that it refers to. The copy
-- has the offsets and the comments of the alias, and no anchor. The nodes
-- inside the copy have no comments, because the comments are at the anchor
-- already. The node must pass 'represent', so every alias refers to an
-- earlier anchor.
expandAliases :: S.Node -> S.Node
expandAliases = fst . go M.empty
  where
    -- An anchor maps to its tag and to its content without comments, which
    -- its aliases share.
    go :: M.Map T.Text (S.Tag, S.Content) -> S.Node -> (S.Node, M.Map T.Text (S.Tag, S.Content))
    go anchors sn = case sn.content of
      S.Alias name -> case M.lookup name anchors of
        Just (tag, content) -> (S.Node sn.offset sn.endOffset (S.Props Nothing tag) sn.comments content, anchors)
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

    define :: S.Node -> M.Map T.Text (S.Tag, S.Content) -> (S.Node, M.Map T.Text (S.Tag, S.Content))
    define sn anchors = case sn.props.anchor of
      Just a -> (sn, M.insert a (sn.props.tag, withoutComments sn.content) anchors)
      Nothing -> (sn, anchors)

    withoutComments :: S.Content -> S.Content
    withoutComments = \case
      S.Sequence style xs -> S.Sequence style (map node xs)
      S.Mapping style kvs -> S.Mapping style [(node k, node v) | (k, v) <- kvs]
      c -> c
      where
        node :: S.Node -> S.Node
        node n = S.Node n.offset n.endOffset n.props S.noComments (withoutComments n.content)

    goList :: M.Map T.Text (S.Tag, S.Content) -> [S.Node] -> ([S.Node], M.Map T.Text (S.Tag, S.Content))
    goList anchors = \case
      [] -> ([], anchors)
      x : xs ->
        let (x', anchors') = go anchors x
            (xs', anchors'') = goList anchors' xs
        in (x' : xs', anchors'')

    goPairs :: M.Map T.Text (S.Tag, S.Content) -> [(S.Node, S.Node)] -> ([(S.Node, S.Node)], M.Map T.Text (S.Tag, S.Content))
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

-- | The keys come with their nodes.
checkUniqueKeys :: [(S.Node, Value)] -> Either Failure ()
checkUniqueKeys keys = case duplicate keys of
  Just (k, first) -> Left $ duplicateKey k first
  Nothing -> Right ()

-- | The error at a key, with a note at the first key that is equal to it.
duplicateKey :: (S.Node, Value) -> (S.Node, Value) -> Failure
duplicateKey (kn, k) (firstNode, first) = (kn.offset, message) NE.:| [(firstNode.offset, note)]
  where
    message :: String
    message = case (k, keyText kn k, keyText firstNode first) of
      (String "<<", _, _) -> "duplicate key \"<<\", " ++ noMergeKeys
      (_, Just t, Just f) | t /= f -> "duplicate key " ++ t ++ ", the same value as the first key"
      (_, Just t, _) -> "duplicate key " ++ t
      (_, Nothing, _) -> "duplicate key"

    note :: String
    note = "the first key" ++ maybe "" (' ' :) (keyText firstNode first)

-- | The hint for a key @<<@. YAML 1.1 used it to merge mappings, and some
-- tools still do, but in YAML 1.2 it is a string.
noMergeKeys :: String
noMergeKeys = "merge keys are not supported"

-- | The key as the input writes it, a string in quotes. A collection and an
-- empty scalar have no text.
keyText :: S.Node -> Value -> Maybe String
keyText n v = case (n.content, v) of
  (S.Alias name, _) -> Just ('*' : T.unpack name)
  (S.Scalar {}, String t) -> Just (show t)
  (S.Scalar _ t, _) | not (T.null t) -> Just (T.unpack t)
  _ -> Nothing

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

-- | The first scalar key that is equal to an earlier one, and the earlier
-- one. A document with a collection key gets numbers for its keys instead.
duplicate :: [(S.Node, Value)] -> Maybe ((S.Node, Value), (S.Node, Value))
duplicate keys = case drop maxPairwise keys of
  _ : _ -> viaMap M.empty keys
  [] -> pairwise [] keys
  where
    -- Comparing all pairs is faster for 16 keys or fewer, by a measurement.
    maxPairwise :: Int
    maxPairwise = 16

    viaMap :: M.Map Value S.Node -> [(S.Node, Value)] -> Maybe ((S.Node, Value), (S.Node, Value))
    viaMap seen = \case
      [] -> Nothing
      k@(n, v) : ks -> case M.lookup v seen of
        Just first -> Just (k, (first, v))
        Nothing -> viaMap (M.insert v n seen) ks

    pairwise :: [(S.Node, Value)] -> [(S.Node, Value)] -> Maybe ((S.Node, Value), (S.Node, Value))
    pairwise seen = \case
      [] -> Nothing
      k@(_, v) : ks -> case L.find ((== v) . snd) seen of
        Just first -> Just (k, first)
        Nothing -> pairwise (k : seen) ks
