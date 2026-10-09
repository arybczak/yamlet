{-# OPTIONS_HADDOCK not-home #-}

-- | The checks of a syntax tree before the decoder reads it, and the values
-- of its nodes.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Compose
  ( prepareWithin
  , aliasLimit
  , representPrepared
  , Failure
  , noMergeKeys
  ) where

import Control.Monad
import Data.Foldable
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T

import Yamlet.Internal.Schema
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Internal.Utils
import Yamlet.Internal.View
import Yamlet.Value

-- | Check that the tags of a node are valid and that the keys of every
-- mapping are unique, and replace each alias with the node that it refers
-- to. The result has no aliases. A node without aliases comes back
-- unchanged.
--
-- The arguments are the limit of the visits that the aliases can add, see
-- 'aliasLimit', and the visits that the aliases of the documents before
-- added. The result also gives the visits that the aliases added with this
-- document.
prepareWithin :: Int -> Int -> S.Node -> Either Failure (S.Node, Int)
prepareWithin limit added root
  | needsNumbering root = (expandAliases root,) <$> numberWithin limit added root
  | otherwise = (root, added) <$ check root
  where
    -- The node has an alias or a collection key inside it.
    needsNumbering :: S.Node -> Bool
    needsNumbering n = case n.content of
      S.ScalarContent {} -> False
      S.SequenceContent _ xs -> any needsNumbering xs
      S.MappingContent _ kvs -> any (\(k, v) -> isCollection k || needsNumbering k || needsNumbering v) kvs
      S.AliasContent {} -> True
      where
        isCollection :: S.Node -> Bool
        isCollection k = case k.content of
          S.SequenceContent {} -> True
          S.MappingContent {} -> True
          _ -> False

-- | The limit of the visits of a traversal that the aliases of the documents
-- can add together: as many visits as the documents have, or a fixed minimum
-- for small documents. A node is one visit and each character of its scalar,
-- tag and anchor is one more, because the decoder copies the texts of each
-- alias. Without a limit, the visits of a small input can be exponential in
-- its size. The documents of a stream share the limit, so that many small
-- documents cannot add the minimum each.
aliasLimit :: [S.Node] -> Int
aliasLimit roots = max minExpansion (sum (map syntaxSize roots))

-- | The visits of a traversal of a node without aliases.
syntaxSize :: S.Node -> Int
syntaxSize n =
  ownVisits n + case n.content of
    S.SequenceContent _ xs -> sum (map syntaxSize xs)
    S.MappingContent _ kvs -> sum [syntaxSize k + syntaxSize v | (k, v) <- kvs]
    _ -> 0

-- | The visits of a node without the nodes inside it.
ownVisits :: S.Node -> Int
ownVisits n = 1 + anchorChars + tagChars + scalarChars
  where
    anchorChars :: Int
    anchorChars = maybe 0 T.length n.props.anchor

    tagChars :: Int
    tagChars = case n.props.tag of
      S.Tag t -> T.length t
      _ -> 0

    scalarChars :: Int
    scalarChars = case n.content of
      S.ScalarContent _ t -> T.length t
      _ -> 0

-- | The checks of 'prepareWithin' for a node with aliases or collection keys,
-- which compare by the numbers of their values, and the visits that the
-- aliases added. The limit is evaluated only at an alias, so that the size
-- of a document without aliases is not computed.
numberWithin :: Int -> Int -> S.Node -> Either Failure Int
numberWithin limit added root = do
  (_, st) <- go (Numbering M.empty M.empty 0 added) root
  Right st.added
  where
    -- Each value comes with its number.
    go :: Numbering -> S.Node -> Either Failure ((Value, Int), Numbering)
    go st sn =
      let off = sn.offset; props = sn.props
      in case sn.content of
           S.AliasContent name -> case M.lookup name st.anchors of
             Just (Just (v, i, visits))
               | st.added + visits > limit ->
                   Left
                     $ failure off
                     $ "the aliases add more than " ++ show limit ++ " nodes and characters"
               | otherwise -> Right ((v, i), st {visits = st.visits + visits, added = st.added + visits})
             Just Nothing ->
               Left
                 $ failure off
                 $ "the alias *" ++ T.unpack name ++ " refers to a node that contains it"
             Nothing ->
               Left
                 $ failure off
                 $ "undefined alias *" ++ T.unpack name
           S.ScalarContent style t -> do
             v <- scalar off props style t
             let visits = ownVisits sn
             Right $ number props v (ScalarShape v) visits visits (open props st)
           S.SequenceContent _ xs -> do
             tag <- collectionTag off props seqTag
             (vs, st') <- goList (open props st) xs
             let v = withTag tag (Sequence (map fst vs))
             let own = ownVisits sn
             Right $ number props v (SequenceShape tag (map snd vs)) own (st'.visits - st.visits + own) st'
           S.MappingContent _ kvs -> do
             tag <- collectionTag off props mapTag
             (entries, st') <- goPairs (open props st) kvs
             checkUniqueNumbers entries
             let v = withTag tag (Mapping [(k, x) | (_, (k, _), (x, _)) <- entries])
                 shape = MappingShape tag (L.sort [(i, j) | (_, (_, i), (_, j)) <- entries])
                 own = ownVisits sn
             Right $ number props v shape own (st'.visits - st.visits + own) st'

    -- The values are in reverse until the end, so that the stack does not
    -- grow with the number of items.
    goList :: Numbering -> [S.Node] -> Either Failure ([(Value, Int)], Numbering)
    goList = loop []
      where
        loop :: [(Value, Int)] -> Numbering -> [S.Node] -> Either Failure ([(Value, Int)], Numbering)
        loop acc st = \case
          [] -> Right (reverse acc, st)
          x : xs -> case go st x of
            Left err -> Left err
            Right (v, st') -> loop (v : acc) st' xs

    -- The entries come with the nodes of their keys, in reverse as in
    -- 'goList'.
    goPairs
      :: Numbering
      -> [(S.Node, S.Node)]
      -> Either Failure ([(S.Node, (Value, Int), (Value, Int))], Numbering)
    goPairs = loop []
      where
        loop
          :: [(S.Node, (Value, Int), (Value, Int))]
          -> Numbering
          -> [(S.Node, S.Node)]
          -> Either Failure ([(S.Node, (Value, Int), (Value, Int))], Numbering)
        loop acc st = \case
          [] -> Right (reverse acc, st)
          (k, v) : kvs -> case go st k of
            Left err -> Left err
            Right (kv, st') -> case go st' v of
              Left err -> Left err
              Right (vv, st'') -> loop ((k, kv, vv) : acc) st'' kvs

    open :: S.Props -> Numbering -> Numbering
    open props st = case props.anchor of
      Just a -> st {anchors = M.insert a Nothing st.anchors}
      Nothing -> st

    -- Give the value the number of its shape, and define its anchor. The
    -- own visits are those of the node alone, and the visits are those of the
    -- node and of everything inside it. The copy at an alias has no anchor,
    -- so its visits leave out the anchor. A node inside with the same anchor
    -- comes later in the document, so its definition stays.
    number :: S.Props -> Value -> Shape -> Int -> Int -> Numbering -> ((Value, Int), Numbering)
    number props v shape own visits st = ((v, i), st {anchors = anchors', shapes = shapes', visits = st.visits + own})
      where
        i :: Int
        shapes' :: M.Map Shape Int
        (i, shapes') = case M.lookup shape st.shapes of
          Just j -> (j, st.shapes)
          Nothing -> let j = M.size st.shapes in (j, M.insert shape j st.shapes)

        anchors' :: M.Map T.Text (Maybe (Value, Int, Int))
        anchors' = case props.anchor of
          Just a | Just Nothing <- M.lookup a st.anchors -> M.insert a (Just (v, i, visits - T.length a)) st.anchors
          _ -> st.anchors

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

-- | The value of a node that passed 'prepareWithin', so it has no aliases and its
-- keys are unique already.
representPrepared :: S.Node -> Either Failure Value
representPrepared = go
  where
    go :: S.Node -> Either Failure Value
    go sn =
      let off = sn.offset; props = sn.props
      in case sn.content of
           S.ScalarContent style t -> scalar off props style t
           S.SequenceContent _ xs -> do
             tag <- collectionTag off props seqTag
             vs <- mapEither go xs
             Right $ withTag tag (Sequence vs)
           S.MappingContent _ kvs -> do
             tag <- collectionTag off props mapTag
             entries <- mapEither (\(k, v) -> (,) <$> go k <*> go v) kvs
             Right $ withTag tag (Mapping entries)
           S.AliasContent _ -> Left $ failure off "unexpected alias"

-- | 'mapM' for 'Either', with a stack that does not grow with the length of
-- the list.
mapEither :: forall a e b. (a -> Either e b) -> [a] -> Either e [b]
mapEither f = go []
  where
    go :: [b] -> [a] -> Either e [b]
    go acc = \case
      [] -> Right (reverse acc)
      x : xs -> case f x of
        Left err -> Left err
        Right y -> go (y : acc) xs

-- | The offset of the node that caused an error and the message, and the
-- notes that go after it, e.g. the first key of a duplicate key.
type Failure = NE.NonEmpty (S.Offset, String)

failure :: S.Offset -> String -> Failure
failure off msg = (off, msg) NE.:| []

-- | The checks of 'numberWithin' for a node without aliases and collection
-- keys.
-- Only the keys get values, for the comparison.
check :: S.Node -> Either Failure ()
check sn =
  let off = sn.offset; props = sn.props
  in case sn.content of
       S.ScalarContent style t
         -- Only a number can fail without a tag.
         | S.NoTag <- props.tag
         , style /= S.Plain || not (maybeNumber t) ->
             Right ()
         | otherwise -> void (scalar off props style t)
       S.SequenceContent _ xs -> collectionTag off props seqTag *> traverse_ check xs
       S.MappingContent _ kvs -> do
         _ <- collectionTag off props mapTag
         keys <- mapEither (\(k, v) -> key k <* check v) kvs
         checkUniqueKeys keys
       S.AliasContent _ -> Left $ failure off "unexpected alias"
  where
    maybeNumber :: T.Text -> Bool
    maybeNumber t = maybe False (startsNumber . fst) (T.uncons t)

    key :: S.Node -> Either Failure (S.Node, Value)
    key k = case k.content of
      S.ScalarContent style t -> (k,) <$> scalar k.offset k.props style t
      _ -> Left $ failure k.offset "unexpected collection key"

    -- The keys come with their nodes.
    checkUniqueKeys :: [(S.Node, Value)] -> Either Failure ()
    checkUniqueKeys keys = case duplicate of
      Just (k, first) -> Left $ duplicateKey k first
      Nothing -> Right ()
      where
        -- The first scalar key that is equal to an earlier one, and the
        -- earlier one. A document with a collection key gets numbers for its
        -- keys instead.
        duplicate :: Maybe ((S.Node, Value), (S.Node, Value))
        duplicate = case drop maxPairwise keys of
          _ : _ -> viaMap M.empty keys
          [] -> pairwise [] keys

        -- Comparing all pairs is faster for 16 keys or fewer, by a
        -- measurement.
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

-- | Replace each alias with a copy of the node that it refers to. The copy
-- has the offsets and the comments of the alias, and no anchor. The nodes
-- inside the copy have the offsets of the alias too, so that an error inside
-- the copy names the place and the path where the document uses the value,
-- and not those of the anchor. They have no comments, because the comments
-- are at the anchor already. The node must pass 'numberWithin', so every
-- alias refers to an earlier anchor.
expandAliases :: S.Node -> S.Node
expandAliases = fst . go M.empty
  where
    go :: M.Map T.Text (S.Tag, S.Content) -> S.Node -> (S.Node, M.Map T.Text (S.Tag, S.Content))
    go anchors sn = case sn.content of
      S.AliasContent name -> case M.lookup name anchors of
        Just (tag, content) -> (S.Node sn.offset sn.endOffset (S.Props Nothing tag) sn.comments (copyAt sn content), anchors)
        Nothing -> (sn, anchors)
      S.ScalarContent {} -> define sn anchors
      S.SequenceContent style xs ->
        let (xs', anchors') = goList (open anchors) xs
        in close (withContent (S.SequenceContent style xs')) anchors'
      S.MappingContent style kvs ->
        let (kvs', anchors') = goPairs (open anchors) kvs
        in close (withContent (S.MappingContent style kvs')) anchors'
      where
        withContent :: S.Content -> S.Node
        withContent = S.Node sn.offset sn.endOffset sn.props sn.comments

        -- No alias inside refers to the anchor, so its old definition can
        -- go. A node inside with the same anchor comes later in the
        -- document, so its definition stays.
        open :: M.Map T.Text (S.Tag, S.Content) -> M.Map T.Text (S.Tag, S.Content)
        open = maybe id M.delete sn.props.anchor

        close :: S.Node -> M.Map T.Text (S.Tag, S.Content) -> (S.Node, M.Map T.Text (S.Tag, S.Content))
        close n anchors' = case sn.props.anchor of
          Just a | M.member a anchors' -> (n, anchors')
          _ -> define n anchors'

    define :: S.Node -> M.Map T.Text (S.Tag, S.Content) -> (S.Node, M.Map T.Text (S.Tag, S.Content))
    define sn anchors = case sn.props.anchor of
      Just a -> (sn, M.insert a (sn.props.tag, sn.content) anchors)
      Nothing -> (sn, anchors)

    -- The content at the alias.
    copyAt :: S.Node -> S.Content -> S.Content
    copyAt alias = \case
      S.SequenceContent style xs -> S.SequenceContent style (map node xs)
      S.MappingContent style kvs -> S.MappingContent style [(node k, node v) | (k, v) <- kvs]
      c -> c
      where
        node :: S.Node -> S.Node
        node n = S.Node alias.offset alias.endOffset n.props S.noComments (copyAt alias n.content)

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
        -- The check comes before the decoder, which knows if the value is
        -- a string. A node that a program built has no input to quote.
        Left _
          | off == S.noOffset -> Left $ failure off exponentOutOfRange
          | otherwise -> Left $ failure off $ exponentOutOfRange ++ ", quote the value if it is a string, e.g. '" ++ T.unpack t ++ "'"
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
                then ", " ++ showText t ++ " is a boolean only in YAML 1.1"
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
  where
    isCoreTag :: T.Text -> Bool
    isCoreTag tag = tag `elem` [nullTag, boolTag, intTag, floatTag, strTag, seqTag, mapTag]

-- | The error at a key, with a note at the first key that is equal to it.
duplicateKey :: (S.Node, Value) -> (S.Node, Value) -> Failure
duplicateKey (kn, k) (firstNode, _) = (kn.offset, message) NE.:| [(firstNode.offset, note)]
  where
    message :: String
    message = case (k, inputText kn, inputText firstNode) of
      (String "<<", _, _) -> "duplicate key \"<<\"" ++ noMergeKeys
      (_, Just t, Just f) | t /= f -> "duplicate key " ++ t ++ ", the same value as the first key"
      (_, Just t, _) -> "duplicate key " ++ t
      (_, Nothing, _) -> "duplicate key"

    note :: String
    note = "the first key" ++ maybe "" (' ' :) (inputText firstNode)

-- | The hint after the message of an error at a key @<<@. YAML 1.1 used it to
-- merge mappings, and some tools still do, but in YAML 1.2 it is a string.
noMergeKeys :: String
noMergeKeys = ", merge keys are not supported"

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
  , added :: !Int
  -- ^ The visits that the aliases added so far, with those of the documents
  -- before in the stream.
  }

-- | A value with the numbers of its items or entries in place of them. The
-- entries of a mapping are sorted, so their order does not matter. A scalar
-- is small, so it stays a value.
data Shape
  = ScalarShape !Value
  | SequenceShape !T.Text ![Int]
  | MappingShape !T.Text ![(Int, Int)]
  deriving stock (Eq, Ord)
