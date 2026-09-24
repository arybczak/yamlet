{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Composition of the representation graph from the syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Compose
  ( compose
  ) where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Exts

import Yamlet.Error
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Node
import Yamlet.Schema

-- | Resolve the tags and the aliases of a document and check that the keys of
-- every mapping are unique. The input is for error messages.
compose :: T.Text -> S.Document -> Either Error Node
compose input doc
  | hasAlias doc.root = fst <$> go M.empty doc.root
  | otherwise = plain doc.root
  where
    -- Without aliases the anchors do not matter.
    plain :: S.Node -> Either Error Node
    plain sn =
      let off = sn.offset; props = sn.props
      in case sn.content of
           S.Scalar style t -> scalar off props style t
           S.Sequence _ xs -> do
             tag <- collectionTag off props seqTag
             ns <- mapM plain xs
             Right $ Node off tag (Sequence ns)
           S.Mapping _ kvs -> do
             tag <- collectionTag off props mapTag
             entries <- mapM (\(k, v) -> (,) <$> plain k <*> plain v) kvs
             checkUniqueKeys entries
             Right $ Node off tag (Mapping entries)
           S.Alias _ -> Left $ errorAt input off "unexpected alias"

    -- An anchor maps to Nothing while the parser composes its node.
    go :: M.Map T.Text (Maybe Node) -> S.Node -> Either Error (Node, M.Map T.Text (Maybe Node))
    go anchors sn =
      let off = sn.offset; props = sn.props
      in case sn.content of
           S.Alias name -> case M.lookup name anchors of
             Just (Just n) -> Right (Node off n.tag n.value, anchors)
             Just Nothing ->
               Left
                 $ errorAt input off
                 $ "the alias *" ++ T.unpack name ++ " refers to a node that contains it"
             Nothing ->
               Left
                 $ errorAt input off
                 $ "undefined alias *" ++ T.unpack name
           S.Scalar style t -> do
             n <- scalar off props style t
             Right (n, define props n anchors)
           S.Sequence _ xs -> do
             tag <- collectionTag off props seqTag
             (ns, anchors') <- goList (open props anchors) xs
             let n = Node off tag (Sequence ns)
             Right (n, define props n anchors')
           S.Mapping _ kvs -> do
             tag <- collectionTag off props mapTag
             (entries, anchors') <- goPairs (open props anchors) kvs
             checkUniqueKeys entries
             let n = Node off tag (Mapping entries)
             Right (n, define props n anchors')

    goList
      :: M.Map T.Text (Maybe Node)
      -> [S.Node]
      -> Either Error ([Node], M.Map T.Text (Maybe Node))
    goList anchors = \case
      [] -> Right ([], anchors)
      x : xs -> do
        (n, anchors') <- go anchors x
        (ns, anchors'') <- goList anchors' xs
        Right (n : ns, anchors'')

    goPairs
      :: M.Map T.Text (Maybe Node)
      -> [(S.Node, S.Node)]
      -> Either Error ([(Node, Node)], M.Map T.Text (Maybe Node))
    goPairs anchors = \case
      [] -> Right ([], anchors)
      (k, v) : kvs -> do
        (kn, anchors') <- go anchors k
        (vn, anchors'') <- go anchors' v
        (ns, anchors''') <- goPairs anchors'' kvs
        Right ((kn, vn) : ns, anchors''')

    open :: S.Props -> M.Map T.Text (Maybe Node) -> M.Map T.Text (Maybe Node)
    open props anchors = case props.anchor of
      Just a -> M.insert a Nothing anchors
      Nothing -> anchors

    define :: S.Props -> Node -> M.Map T.Text (Maybe Node) -> M.Map T.Text (Maybe Node)
    define props n anchors = case props.anchor of
      Just a -> M.insert a (Just n) anchors
      Nothing -> anchors

    scalar :: S.Offset -> S.Props -> S.ScalarStyle -> T.Text -> Either Error Node
    scalar off props style t = case props.tag of
      S.NoTag
        | style == S.Plain -> Right $ node' (resolvePlain t)
        | otherwise -> Right $ Node off strTag (String t)
      S.NonSpecificTag -> Right $ Node off strTag (String t)
      S.Tag tag
        | tag == seqTag || tag == mapTag ->
            Left
              $ errorAt input off
              $ "the tag !!" ++ T.unpack (T.drop 18 tag) ++ " cannot be used on a scalar"
        | otherwise -> case resolveTagged tag t of
            Just v -> Right $ Node off tag v
            Nothing ->
              Left
                $ errorAt input off
                $ "invalid value for the tag !!" ++ T.unpack (T.drop 18 tag)
      where
        node' :: Value -> Node
        node' v = Node off (defaultTag v) v

    collectionTag :: S.Offset -> S.Props -> T.Text -> Either Error T.Text
    collectionTag off props def = case props.tag of
      S.NoTag -> Right def
      S.NonSpecificTag -> Right def
      S.Tag tag
        | tag == def || not (isCoreTag tag) -> Right tag
        | otherwise ->
            Left
              $ errorAt input off
              $ "the tag !!"
                ++ T.unpack (T.drop 18 tag)
                ++ " cannot be used on a "
                ++ (if def == seqTag then "sequence" else "mapping")

    isCoreTag :: T.Text -> Bool
    isCoreTag tag = tag `elem` [nullTag, boolTag, intTag, floatTag, strTag, seqTag, mapTag]

    checkUniqueKeys :: [(Node, Node)] -> Either Error ()
    checkUniqueKeys entries = case duplicate (map fst entries) of
      Just k -> Left $ errorAt input k.offset $ case k.value of
        String t -> "duplicate key " ++ show t
        _ -> "duplicate key"
      Nothing -> Right ()

hasAlias :: S.Node -> Bool
hasAlias n = case n.content of
  S.Scalar {} -> False
  S.Sequence _ xs -> any hasAlias xs
  S.Mapping _ kvs -> any (\(k, v) -> hasAlias k || hasAlias v) kvs
  S.Alias {} -> True

-- | The first key that is equal to an earlier one.
duplicate :: [Node] -> Maybe Node
duplicate keys = case keys of
  -- Comparing all pairs is faster for few keys.
  _ : _ : _ : _ : _ : _ : _ : _ : _ -> viaSet Set.empty keys
  _ -> pairwise [] keys
  where
    viaSet :: Set.Set Key -> [Node] -> Maybe Node
    viaSet seen = \case
      [] -> Nothing
      k : ks
        | Key k `Set.member` seen -> Just k
        | otherwise -> viaSet (Set.insert (Key k) seen) ks

    pairwise :: [Node] -> [Node] -> Maybe Node
    pairwise seen = \case
      [] -> Nothing
      k : ks
        | any (sameNode k) seen -> Just k
        | otherwise -> pairwise (k : seen) ks

newtype Key = Key Node

instance Eq Key where
  Key a == Key b = sameNode a b

instance Ord Key where
  compare (Key a) (Key b) = compareNodes a b

-- | Equality of nodes that ignores their offsets. It is faster than
-- 'compareNodes' for the few keys of most mappings.
sameNode :: Node -> Node -> Bool
sameNode (Node _ tagA valueA) (Node _ tagB valueB) =
  tagA == tagB
    && ( isTrue# (reallyUnsafePtrEquality# valueA valueB) || case (valueA, valueB) of
           (Sequence xs, Sequence ys) -> length xs == length ys && and (zipWith sameNode xs ys)
           (Mapping xs, Mapping ys) -> length xs == length ys && all (\(k, v) -> any (samePair k v) ys) xs
           (x, y) -> x == y
       )
  where
    samePair :: Node -> Node -> (Node, Node) -> Bool
    samePair k v (k', v') = sameNode k k' && sameNode v v'

-- | An order of nodes that ignores their offsets and the order of the entries
-- of a mapping. The aliases of an anchor share the value of its node, so the
-- order and 'sameNode' take such values as equal without a look inside, even
-- if an alias expands to a huge node.
compareNodes :: Node -> Node -> Ordering
-- A pattern binds the evaluated fields, a selector would give new thunks.
compareNodes (Node _ tagA valueA) (Node _ tagB valueB)
  | isTrue# (reallyUnsafePtrEquality# valueA valueB) = compare tagA tagB
  -- The values of keys differ more often than their tags.
  | otherwise = compareValues valueA valueB <> compare tagA tagB
  where
    compareValues :: Value -> Value -> Ordering
    compareValues x y = case (x, y) of
      (Null, Null) -> EQ
      (Bool p, Bool q) -> compare p q
      (Int i, Int j) -> compare i j
      (Float f, Float g) -> compare f g
      (String s, String t) -> compare s t
      (Sequence xs, Sequence ys) ->
        compare (length xs) (length ys) <> mconcat (zipWith compareNodes xs ys)
      (Mapping xs, Mapping ys) ->
        compare (length xs) (length ys) <> mconcat (zipWith compareEntries (sorted xs) (sorted ys))
      _ -> compare (rank x) (rank y)

    -- The keys of a mapping are unique, so their order is the same for equal
    -- mappings.
    sorted :: [(Node, Node)] -> [(Node, Node)]
    sorted = L.sortBy (\(k, _) (k', _) -> compareNodes k k')

    compareEntries :: (Node, Node) -> (Node, Node) -> Ordering
    compareEntries (k, v) (k', v') = compareNodes k k' <> compareNodes v v'

    rank :: Value -> Int
    rank = \case
      Null -> 0
      Bool _ -> 1
      Int _ -> 2
      Float _ -> 3
      String _ -> 4
      Sequence _ -> 5
      Mapping _ -> 6
