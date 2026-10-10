-- | The properties of the encoder on random values and syntax trees.
module Yamlet.Test.Encode.Properties
  ( prop_fastRenderer
  , prop_fastRendererAll
  , prop_fastRendererNodes
  , prop_roundTrip
  , prop_syntaxRoundTrip
  ) where

import Data.List qualified as L
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Test.Tasty.QuickCheck

import Yamlet
import Yamlet.Syntax qualified as S

-- | The faster renderer of the encoder gives the same output as the renderer
-- of syntax trees.
prop_fastRenderer :: Doc -> Property
prop_fastRenderer (Doc n) =
  encodeText n === S.renderSyntax S.defaultRenderOptions [S.document (toYaml n)]

-- | The same for several documents.
prop_fastRendererAll :: [Doc] -> Property
prop_fastRendererAll docs =
  encodeAllText ns
    === S.renderSyntax S.defaultRenderOptions (map (S.document . toYaml) ns)
  where
    ns :: [Value]
    ns = [n | Doc n <- docs]

-- | The same for syntax trees that the faster renderer takes, with what the
-- trees of values do not have: tags on keys and on collections, scalar keys
-- in each style and keys of more than 1024 characters.
prop_fastRendererNodes :: SimpleNode -> Property
prop_fastRendererNodes (SimpleNode n) =
  encodeText n === S.renderSyntax S.defaultRenderOptions [S.document n]

-- | A tree without comments, anchors, aliases and flow collections, with
-- scalars on one line in the styles of the encoder.
newtype SimpleNode = SimpleNode S.Node
  deriving stock (Show)

instance Arbitrary SimpleNode where
  arbitrary = SimpleNode <$> sized genNode
    where
      genNode :: Int -> Gen S.Node
      genNode size
        | size <= 1 = genScalar
        | otherwise =
            frequency
              [ (3, genScalar)
              , (1, collection S.sequenceNode (genNode (size `div` 3)))
              ,
                ( 1
                , collection
                    S.mappingNode
                    ((,) <$> genNode (size `div` 3) <*> genNode (size `div` 3))
                )
              , (1, elements [S.sequenceNode [], S.mappingNode []] >>= withTag)
              ]

      collection :: ([a] -> S.Node) -> Gen a -> Gen S.Node
      collection node item = do
        k <- choose (1, 3)
        xs <- vectorOf k item
        withTag (node xs)

      genScalar :: Gen S.Node
      genScalar = do
        style <- elements [S.Plain, S.SingleQuoted, S.DoubleQuoted, S.Literal]
        t <- genText
        -- The faster renderer does not take an empty plain scalar.
        withTag (S.scalarNode style (if style == S.Plain && T.null t then "x" else t))

      withTag :: S.Node -> Gen S.Node
      withTag n = do
        tag <-
          frequency
            [ (4, pure S.NoTag)
            , (1, S.Tag <$> elements ["!t", "xy", "tag:yaml.org,2002:str", "", "a#b"])
            ]
        pure n {S.props = S.Props Nothing tag}

      genText :: Gen T.Text
      genText =
        oneof
          [ elements
              [ ""
              , " "
              , " a"
              , "\ta"
              , "a\n"
              , "\n"
              , " lead\nx"
              , "-"
              , "a: b"
              , "#"
              , "yes"
              , "x'y"
              , "a\x2028b"
              , "a\x01"
              , "---"
              , "a\n\n"
              ]
          , T.pack <$> listOf (elements "ab :#\n\t'\"-")
          , pure (T.replicate 1030 "k")
          ]

-- | Encoding a value and decoding the result gives the same value.
prop_roundTrip :: Doc -> Property
prop_roundTrip (Doc n) = readsBack (encodeText n) n

-- | Rendering the syntax tree of a value and decoding the result gives the
-- same value.
prop_syntaxRoundTrip :: Doc -> Property
prop_syntaxRoundTrip (Doc n) = readsBack output n
  where
    output :: T.Text
    output = S.renderSyntax S.defaultRenderOptions [S.document (toYaml n)]

readsBack :: T.Text -> Value -> Property
readsBack output n = case decodeAllText @Value output of
  Right [n'] -> counterexample (T.unpack output) $ n' === n
  r -> counterexample (T.unpack output ++ "\n" ++ show r) False

newtype Doc = Doc Value
  deriving stock (Show)

instance Arbitrary Doc where
  arbitrary = Doc <$> sized genValue

genValue :: Int -> Gen Value
genValue size
  | size <= 1 = genScalar
  | otherwise =
      frequency
        [ (3, genScalar)
        , (1, Sequence <$> genList)
        , (1, Mapping <$> genEntries)
        , (1, tagged <$> genScalar)
        , (1, Tagged <$> genTag <*> (Sequence <$> genList))
        ]
  where
    genList :: Gen [Value]
    genList = do
      k <- choose (0, 4)
      vectorOf k (genValue (size `div` 3))

    genEntries :: Gen [(Value, Value)]
    genEntries = do
      k <- choose (0, 4)
      keys <- L.nub <$> vectorOf k genKey
      mapM (\key -> (key,) <$> genValue (size `div` 3)) keys

    genKey :: Gen Value
    genKey = frequency [(4, genScalar), (1, elements [Sequence [], Mapping []])]

    -- A tag that is not a valid URI needs a %TAG directive.
    genTag :: Gen T.Text
    genTag = elements ["!custom", "xy"]

    tagged :: Value -> Value
    tagged v = case v of
      String _ -> Tagged "!custom" v
      _ -> v

    genScalar :: Gen Value
    genScalar =
      oneof
        [ pure Null
        , Bool <$> arbitrary
        , Int <$> arbitrary
        , Float . Finite <$> (Sci.scientific <$> arbitrary <*> chooseInt (-30, 30))
        , Float <$> elements [NegativeZero, Infinity, NegativeInfinity, NaN]
        , String <$> genText
        ]

    genText :: Gen T.Text
    genText =
      oneof
        [ elements tricky
        , T.pack <$> listOf genChar
        , T.intercalate "\n" <$> listOf (T.pack <$> listOf genChar)
        ]
      where
        tricky :: [T.Text]
        tricky =
          [ ""
          , " "
          , "-"
          , "- a"
          , "? a"
          , ": a"
          , "a: b"
          , "a:b"
          , "#"
          , "a #b"
          , "true"
          , "null"
          , "1"
          , "0x1F"
          , "0o7"
          , ".5"
          , "~"
          , "---"
          , "..."
          , "@x"
          , "`x"
          , "foo\n"
          , "\nfoo"
          , "  lead"
          , "trail  "
          , "a\n\nb\n\n"
          , "\t"
          , "é"
          , "\x85"
          , "\x2028"
          , "\xFEFF"
          , "\n"
          , "\n\n"
          , " \n"
          , "a\n "
          , "|"
          , ">"
          , "%x"
          , "&a"
          , "*a"
          , "!a"
          , "{}"
          , "[]"
          , "a, b"
          , "key:"
          , "'quoted'"
          , "\"dq\""
          , "\r\n"
          , "\\"
          , "a\tb"
          ]

        genChar :: Gen Char
        genChar =
          frequency
            [ (10, elements "abc xyz-:#,[]{}'\"!&*?|>%@`\\")
            , (2, elements "\t\r\x85\xA0\x2028\xFEFF\x01\x7F")
            , (1, arbitrary)
            ]
