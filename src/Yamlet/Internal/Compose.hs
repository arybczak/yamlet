{-# OPTIONS_HADDOCK not-home #-}
-- | Composition of the representation graph from the syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Compose
  ( compose
  ) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T

import Yamlet.Error
import Yamlet.Internal.Schema
import Yamlet.Node
import Yamlet.Syntax qualified as S

-- | Resolve the tags and the aliases of a document and check that the keys of
-- every mapping are unique. The input is for error messages.
compose :: T.Text -> S.Document -> Either Error Node
compose input doc
  | hasAlias doc.root = fst <$> go M.empty doc.root
  | otherwise = plain doc.root
  where
    -- Without aliases the anchors do not matter.
    plain :: S.Node -> Either Error Node
    plain = \case
      S.Scalar off props style t -> scalar off props style t
      S.Sequence off props _ xs -> do
        tag <- collectionTag off props seqTag
        ns <- mapM plain xs
        Right $ Node off tag (Sequence ns)
      S.Mapping off props _ kvs -> do
        tag <- collectionTag off props mapTag
        entries <- mapM (\(k, v) -> (,) <$> plain k <*> plain v) kvs
        checkUniqueKeys entries
        Right $ Node off tag (Mapping entries)
      S.Alias off _ -> Left $ errorAt input off "unexpected alias"

    -- An anchor maps to Nothing while the parser composes its node.
    go :: M.Map T.Text (Maybe Node) -> S.Node -> Either Error (Node, M.Map T.Text (Maybe Node))
    go anchors = \case
      S.Alias off name -> case M.lookup name anchors of
        Just (Just n) -> Right (Node off n.tag n.value, anchors)
        Just Nothing -> Left $ errorAt input off $
          "the alias *" ++ T.unpack name ++ " refers to a node that contains it"
        Nothing -> Left $ errorAt input off $
          "undefined alias *" ++ T.unpack name
      S.Scalar off props style t -> do
        n <- scalar off props style t
        Right (n, define props n anchors)
      S.Sequence off props _ xs -> do
        tag <- collectionTag off props seqTag
        (ns, anchors') <- goList (open props anchors) xs
        let n = Node off tag (Sequence ns)
        Right (n, define props n anchors')
      S.Mapping off props _ kvs -> do
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
        | tag == seqTag || tag == mapTag -> Left $ errorAt input off $
            "the tag !!" ++ T.unpack (T.drop 18 tag) ++ " cannot be used on a scalar"
        | otherwise -> case resolveTagged tag t of
            Just v -> Right $ Node off tag v
            Nothing -> Left $ errorAt input off $
              "invalid value for the tag !!" ++ T.unpack (T.drop 18 tag)
      where
        node' :: Value -> Node
        node' v = Node off (defaultTag v) v

    collectionTag :: S.Offset -> S.Props -> T.Text -> Either Error T.Text
    collectionTag off props def = case props.tag of
      S.NoTag -> Right def
      S.NonSpecificTag -> Right def
      S.Tag tag
        | tag == def || not (isCoreTag tag) -> Right tag
        | otherwise -> Left $ errorAt input off $
            "the tag !!" ++ T.unpack (T.drop 18 tag) ++ " cannot be used on a "
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
hasAlias = \case
  S.Scalar{} -> False
  S.Sequence _ _ _ xs -> any hasAlias xs
  S.Mapping _ _ _ kvs -> any (\(k, v) -> hasAlias k || hasAlias v) kvs
  S.Alias{} -> True

-- | The first key that is equal to an earlier one.
duplicate :: [Node] -> Maybe Node
duplicate keys
  | all isScalar keys = case keys of
      -- Comparing all pairs is faster for few keys.
      _ : _ : _ : _ : _ : _ : _ : _ : _ -> viaMap M.empty keys
      _ -> pairwise [] keys
  | otherwise = pairwise [] keys
  where
    isScalar :: Node -> Bool
    isScalar k = case k.value of
      Sequence _ -> False
      Mapping _ -> False
      _ -> True

    viaMap :: M.Map (T.Text, ScalarKey) () -> [Node] -> Maybe Node
    viaMap seen = \case
      [] -> Nothing
      k : ks ->
        let key = (k.tag, scalarKey k.value)
        in if M.member key seen then Just k else viaMap (M.insert key () seen) ks

    pairwise :: [Node] -> [Node] -> Maybe Node
    pairwise seen = \case
      [] -> Nothing
      k : ks
        | any (sameNode k) seen -> Just k
        | otherwise -> pairwise (k : seen) ks

data ScalarKey
  = KNull
  | KBool !Bool
  | KInt !Integer
  | KFloat !Double
  | KString !T.Text
  deriving stock (Eq, Ord)

scalarKey :: Value -> ScalarKey
scalarKey = \case
  Null -> KNull
  Bool b -> KBool b
  Int i -> KInt i
  Float d -> KFloat d
  String t -> KString t
  _ -> KNull

-- | Equality of nodes that ignores their offsets.
sameNode :: Node -> Node -> Bool
sameNode a b = a.tag == b.tag && case (a.value, b.value) of
  (Sequence xs, Sequence ys) -> length xs == length ys && and (zipWith sameNode xs ys)
  (Mapping xs, Mapping ys) -> length xs == length ys && all (\(k, v) -> any (samePair k v) ys) xs
  (x, y) -> x == y
  where
    samePair :: Node -> Node -> (Node, Node) -> Bool
    samePair k v (k', v') = sameNode k k' && sameNode v v'

