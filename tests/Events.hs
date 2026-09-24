-- | A YAML stream as a sequence of events, the representation that the YAML
-- specification uses to describe the result of parsing.
module Events
  ( Event (..)
  , toEvents
  ) where

import Data.Text qualified as T

import Yamlet.Syntax

data Event
  = StreamStart
  | StreamEnd
  | -- | The document starts with a @---@ marker.
    DocumentStart !Bool
  | -- | The document ends with a @...@ marker.
    DocumentEnd !Bool
  | SequenceStart !Props !CollectionStyle
  | SequenceEnd
  | MappingStart !Props !CollectionStyle
  | MappingEnd
  | ScalarEvent !Props !ScalarStyle !T.Text
  | AliasEvent !T.Text
  deriving stock (Eq, Show)

-- | The events of a stream.
toEvents :: [Document] -> [Event]
toEvents docs = StreamStart : foldr documentEvents [StreamEnd] docs
  where
    documentEvents :: Document -> [Event] -> [Event]
    documentEvents doc rest =
      DocumentStart doc.explicitStart
        : node doc.root (DocumentEnd doc.explicitEnd : rest)

    node :: Node -> [Event] -> [Event]
    node n rest = case n.content of
      Scalar style t -> ScalarEvent n.props style t : rest
      Sequence style xs ->
        SequenceStart n.props style : foldr node (SequenceEnd : rest) xs
      Mapping style kvs ->
        MappingStart n.props style : foldr pair (MappingEnd : rest) kvs
      Alias name -> AliasEvent name : rest

    pair :: (Node, Node) -> [Event] -> [Event]
    pair (k, v) rest = node k (node v rest)
