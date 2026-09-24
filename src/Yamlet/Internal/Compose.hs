{-# OPTIONS_HADDOCK not-home #-}

-- | Composition of the representation graph from the syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Compose
  ( compose
  ) where

import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T

import Yamlet.Error
import Yamlet.Internal.Schema
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Node

-- | Resolve the tags and the aliases of a document and check that the keys of
-- every mapping are unique. The input is for error messages.
compose :: T.Text -> S.Document -> Either Error Node
compose input doc
  | needsNumbering doc.root = fst . fst <$> go (Numbering M.empty M.empty) doc.root
  | otherwise = plain doc.root
  where
    -- Without aliases the anchors do not matter, and without collection keys
    -- only scalar keys compare.
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

    -- Each node comes with its number.
    go :: Numbering -> S.Node -> Either Error ((Node, Int), Numbering)
    go st sn =
      let off = sn.offset; props = sn.props
      in case sn.content of
           S.Alias name -> case M.lookup name st.anchors of
             Just (Just (n, i)) -> Right ((Node off n.tag n.value, i), st)
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
             Right $ number props n (ScalarShape (Key n)) st
           S.Sequence _ xs -> do
             tag <- collectionTag off props seqTag
             (ns, st') <- goList (open props st) xs
             Right $ number props (Node off tag (Sequence (map fst ns))) (SequenceShape tag (map snd ns)) st'
           S.Mapping _ kvs -> do
             tag <- collectionTag off props mapTag
             (entries, st') <- goPairs (open props st) kvs
             checkUniqueNumbers entries
             let n = Node off tag (Mapping [(k, v) | ((k, _), (v, _)) <- entries])
             Right $ number props n (MappingShape tag (L.sort [(i, j) | ((_, i), (_, j)) <- entries])) st'

    goList :: Numbering -> [S.Node] -> Either Error ([(Node, Int)], Numbering)
    goList st = \case
      [] -> Right ([], st)
      x : xs -> do
        (n, st') <- go st x
        (ns, st'') <- goList st' xs
        Right (n : ns, st'')

    goPairs
      :: Numbering
      -> [(S.Node, S.Node)]
      -> Either Error ([((Node, Int), (Node, Int))], Numbering)
    goPairs st = \case
      [] -> Right ([], st)
      (k, v) : kvs -> do
        (kn, st') <- go st k
        (vn, st'') <- go st' v
        (ns, st''') <- goPairs st'' kvs
        Right ((kn, vn) : ns, st''')

    open :: S.Props -> Numbering -> Numbering
    open props st = case props.anchor of
      Just a -> st {anchors = M.insert a Nothing st.anchors}
      Nothing -> st

    -- Give the node the number of its shape, and define its anchor.
    number :: S.Props -> Node -> Shape -> Numbering -> ((Node, Int), Numbering)
    number props n shape st = ((n, i), Numbering anchors' shapes')
      where
        i :: Int
        shapes' :: M.Map Shape Int
        (i, shapes') = case M.lookup shape st.shapes of
          Just j -> (j, st.shapes)
          Nothing -> let j = M.size st.shapes in (j, M.insert shape j st.shapes)

        anchors' :: M.Map T.Text (Maybe (Node, Int))
        anchors' = case props.anchor of
          Just a -> M.insert a (Just (n, i)) st.anchors
          Nothing -> st.anchors

    checkUniqueNumbers :: [((Node, Int), (Node, Int))] -> Either Error ()
    checkUniqueNumbers = loop IS.empty
      where
        loop :: IS.IntSet -> [((Node, Int), (Node, Int))] -> Either Error ()
        loop seen = \case
          [] -> Right ()
          ((k, i), _) : rest
            | i `IS.member` seen -> Left $ duplicateKey k
            | otherwise -> loop (IS.insert i seen) rest

    scalar :: S.Offset -> S.Props -> S.ScalarStyle -> T.Text -> Either Error Node
    scalar off props style t = case props.tag of
      S.NoTag
        | style == S.Plain -> case resolvePlainExact t of
            Right v -> Right $ node' v
            Left _ -> Left $ errorAt input off inexact
        | otherwise -> Right $ Node off strTag (String t)
      S.NonSpecificTag -> Right $ Node off strTag (String t)
      S.Tag tag
        | tag == seqTag || tag == mapTag ->
            Left
              $ errorAt input off
              $ "the tag !!" ++ T.unpack (T.drop 18 tag) ++ " cannot be used on a scalar"
        | otherwise -> case resolveTaggedExact tag t of
            Just (Right v) -> Right $ Node off tag v
            Just (Left _) -> Left $ errorAt input off inexact
            Nothing ->
              Left
                $ errorAt input off
                $ "invalid value for the tag !!" ++ T.unpack (T.drop 18 tag)
      where
        node' :: Value -> Node
        node' v = Node off (defaultTag v) v

        inexact :: String
        inexact = "the exponent of the number is out of range"

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
      Just k -> Left $ duplicateKey k
      Nothing -> Right ()

    duplicateKey :: Node -> Error
    duplicateKey k = errorAt input k.offset $ case k.value of
      String t -> "duplicate key " ++ show t
      _ -> "duplicate key"

-- | The state of the composition of a document with aliases or collection
-- keys. Equal nodes get the same number, so that keys compare in constant
-- time, even if they are large collections or come from aliases that expand
-- to huge nodes.
data Numbering = Numbering
  { anchors :: !(M.Map T.Text (Maybe (Node, Int)))
  -- ^ An anchor maps to Nothing while its node is composed.
  , shapes :: !(M.Map Shape Int)
  }

-- | A node with the numbers of its items or entries in place of them. The
-- entries of a mapping are sorted, so their order does not matter.
data Shape
  = ScalarShape !Key
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
        | any (\s -> Key k == Key s) seen -> Just k
        | otherwise -> pairwise (k : seen) ks

-- | A scalar node that ignores its offset. The instances are for scalars only,
-- because a document with a collection key gets numbers for its keys.
newtype Key = Key Node

-- | It is faster than the order for the few keys of most mappings.
instance Eq Key where
  Key (Node _ tagA valueA) == Key (Node _ tagB valueB) = tagA == tagB && valueA == valueB

instance Ord Key where
  -- A pattern binds the evaluated fields, a selector would give new thunks.
  compare (Key (Node _ tagA valueA)) (Key (Node _ tagB valueB)) =
    -- The values of keys differ more often than their tags.
    compareValues valueA valueB <> compare tagA tagB
    where
      compareValues :: Value -> Value -> Ordering
      compareValues x y = case (x, y) of
        (Null, Null) -> EQ
        (Bool p, Bool q) -> compare p q
        (Int i, Int j) -> compare i j
        (Float f, Float g) -> compare f g
        (String s, String t) -> compare s t
        _ -> compare (rank x) (rank y)

      rank :: Value -> Int
      rank = \case
        Null -> 0
        Bool _ -> 1
        Int _ -> 2
        Float _ -> 3
        String _ -> 4
        Sequence _ -> 5
        Mapping _ -> 6
