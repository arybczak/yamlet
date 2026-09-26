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
import Yamlet.Internal.Utils
import Yamlet.Node

-- | Resolve the tags and the aliases of a document and check that the keys of
-- every mapping are unique. The input is for error messages.
compose :: T.Text -> S.Document -> Either Error Node
compose input doc
  | needsNumbering doc.root = fst . fst <$> go (Numbering M.empty M.empty 0) doc.root
  | otherwise = plain doc.root
  where
    -- The limit of the visits of a traversal of the document. Aliases can add
    -- as many visits as the document has nodes, or 'smallBudget' for a small
    -- document. Without a limit, the visits of a small input can be
    -- exponential in its size.
    limit :: Int
    limit = n + max smallBudget n
      where
        n :: Int
        n = syntaxSize doc.root

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
             Just (Just (n, i, visits))
               | st.visits + visits > limit ->
                   Left
                     $ errorAt input off
                     $ "the aliases expand the document to more than " ++ show limit ++ " nodes"
               | otherwise -> Right ((Node off n.tag n.value, i), st {visits = st.visits + visits})
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
             Right $ number props n (ScalarShape (Key n)) 1 st
           S.Sequence _ xs -> do
             tag <- collectionTag off props seqTag
             (ns, st') <- goList (open props st) xs
             let n = Node off tag (Sequence (map fst ns))
             Right $ number props n (SequenceShape tag (map snd ns)) (st'.visits - st.visits + 1) st'
           S.Mapping _ kvs -> do
             tag <- collectionTag off props mapTag
             (entries, st') <- goPairs (open props st) kvs
             checkUniqueNumbers entries
             let n = Node off tag (Mapping [(k, v) | ((k, _), (v, _)) <- entries])
                 shape = MappingShape tag (L.sort [(i, j) | ((_, i), (_, j)) <- entries])
             Right $ number props n shape (st'.visits - st.visits + 1) st'

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

    -- Give the node the number of its shape, and define its anchor. The
    -- visits are those of the node and of everything inside it.
    number :: S.Props -> Node -> Shape -> Int -> Numbering -> ((Node, Int), Numbering)
    number props n shape visits st = ((n, i), Numbering anchors' shapes' (st.visits + 1))
      where
        i :: Int
        shapes' :: M.Map Shape Int
        (i, shapes') = case M.lookup shape st.shapes of
          Just j -> (j, st.shapes)
          Nothing -> let j = M.size st.shapes in (j, M.insert shape j st.shapes)

        anchors' :: M.Map T.Text (Maybe (Node, Int, Int))
        anchors' = case props.anchor of
          Just a -> M.insert a (Just (n, i, visits)) st.anchors
          Nothing -> st.anchors

    -- Unlike in 'duplicate', comparing all pairs is not faster for few keys.
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
              $ "the tag !!" ++ T.unpack (T.drop (T.length coreTagPrefix) tag) ++ " cannot be used on a scalar"
        | otherwise -> case resolveTaggedExact tag t of
            Just (Right v) -> Right $ Node off tag v
            Just (Left _) -> Left $ errorAt input off inexact
            Nothing ->
              Left
                $ errorAt input off
                $ "invalid value for the tag !!"
                  ++ T.unpack (T.drop (T.length coreTagPrefix) tag)
                  ++ if tag == boolTag && isYaml11Bool t
                    then ", " ++ show t ++ " is a boolean only in YAML 1.1"
                    else ""
      where
        node' :: Value -> Node
        node' v = Node off (defaultTag v) v

        inexact :: String
        inexact = "the exponent of the number is out of the range from -1000 to 1000"

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
                ++ T.unpack (T.drop (T.length coreTagPrefix) tag)
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
      -- YAML 1.1 used "<<" to merge mappings, and some tools still do.
      String "<<" -> "duplicate key \"<<\", merge keys are not supported"
      String t -> "duplicate key " ++ show t
      _ -> "duplicate key"

-- | The state of the composition of a document with aliases or collection
-- keys. Equal nodes get the same number, so that keys compare in constant
-- time, even if they are large collections or come from aliases that expand
-- to huge nodes.
data Numbering = Numbering
  { anchors :: !(M.Map T.Text (Maybe (Node, Int, Int)))
  -- ^ An anchor maps to its node, the number of the node and the visits of a
  -- traversal of the node. It maps to Nothing while its node is composed.
  , shapes :: !(M.Map Shape Int)
  , visits :: !Int
  -- ^ The visits of a traversal of the nodes so far.
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
duplicate keys = case drop 16 keys of
  -- Comparing all pairs is faster for 16 keys or fewer.
  _ : _ -> viaSet Set.empty keys
  [] -> pairwise [] keys
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
