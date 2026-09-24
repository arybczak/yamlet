-- | A YAML stream as a sequence of events, the representation that the YAML
-- specification uses to describe the result of parsing.
module Yamlet.Event
  ( Event(..)
  , toEvents
  ) where

import Data.Text qualified as T

import Yamlet.Internal.Syntax

data Event
  = StreamStart
  | StreamEnd
  | DocumentStart !Bool
  -- ^ The document starts with a @---@ marker.
  | DocumentEnd !Bool
  -- ^ The document ends with a @...@ marker.
  | SequenceStart !Props !CollectionStyle
  | SequenceEnd
  | MappingStart !Props !CollectionStyle
  | MappingEnd
  | ScalarEvent !Props !ScalarStyle !T.Text
  | AliasEvent !T.Text
  deriving stock (Eq, Show)

-- | The events of a stream.
toEvents :: [Document] -> [Event]
toEvents docs = StreamStart : foldr document [StreamEnd] docs
  where
    document :: Document -> [Event] -> [Event]
    document doc rest = DocumentStart doc.explicitStart
      : node doc.root (DocumentEnd doc.explicitEnd : rest)

    node :: Node -> [Event] -> [Event]
    node n rest = case n of
      Scalar _ props style t -> ScalarEvent props style t : rest
      Sequence _ props style xs ->
        SequenceStart props style : foldr node (SequenceEnd : rest) xs
      Mapping _ props style kvs ->
        MappingStart props style : foldr pair (MappingEnd : rest) kvs
      Alias _ name -> AliasEvent name : rest

    pair :: (Node, Node) -> [Event] -> [Event]
    pair (k, v) rest = node k (node v rest)
