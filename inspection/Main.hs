{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}

-- | The derived instances contain no generic representation.
--
-- The module exports every binding, because GHC 9.14 removes an unused
-- binding before the plugin checks it.
module Main where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Test.Inspection
import Test.Tasty
import Test.Tasty.HUnit

import Obligations
import Yamlet

main :: IO ()
main =
  defaultMain $
    testGroup
      "Inspection"
      [ testGroup
          "products"
          [ testCase "encode Server" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeServer)
          , testCase "decode Server" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeServer)
          , testCase "encode Wide" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeWide)
          , testCase "decode Wide" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeWide)
          , testCase "encode Name" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeName)
          , testCase "decode Name" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeName)
          , testCase "encode Box" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeBox)
          , testCase "decode Box" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeBox)
          , testCase "encode Velocity" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeVelocity)
          , testCase "decode Velocity" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeVelocity)
          , testCase "encode Config" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeConfig)
          , testCase "decode Config" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeConfig)
          , testCase "encode Preset" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodePreset)
          , testCase "decode Preset" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodePreset)
          ]
      , testGroup
          "sums"
          [ testCase "encode Turn" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeTurn)
          , testCase "decode Turn" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeTurn)
          , testCase "encode Shape" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeShape)
          , testCase "decode Shape" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeShape)
          , testCase "decode Step" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeStep)
          , testCase "encode Figure" $ assertSuccess $(inspectTest $ hasNoGenericRep 'encodeFigure)
          , testCase "decode Figure" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeFigure)
          ]
      , testGroup
          "lists and fields"
          [ testCase "encode a list of Server" $ assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeServers)
          , testCase "encode a field of Server" $ assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeServerField)
          , testCase "encode a list of Shape" $ assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeShapes)
          , testCase "encode a field of Shape" $ assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeShapeField)
          , testCase "decode a field of Server" $ assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeServerField)
          , testCase "decode a field of Shape" $ assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeShapeField)
          ]
      ]

----------------------------------------
-- Products

data Server = Server {host :: T.Text, port :: Int, tags :: Maybe [T.Text]}
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Server

data Wide = Wide
  { i00 :: Int
  , i01 :: Int
  , i02 :: Int
  , i03 :: Int
  , i04 :: Int
  , i05 :: Int
  , i06 :: Int
  , i07 :: Int
  , i08 :: Int
  , i09 :: Int
  , i10 :: Int
  , i11 :: Int
  , i12 :: Int
  , i13 :: Int
  , i14 :: Int
  , i15 :: Int
  , i16 :: Int
  , i17 :: Int
  , i18 :: Int
  , i19 :: Int
  , i20 :: Int
  , i21 :: Int
  , i22 :: Int
  , i23 :: Int
  , i24 :: Int
  , i25 :: Int
  , i26 :: Int
  , i27 :: Int
  , i28 :: Int
  , i29 :: Int
  , i30 :: Int
  , i31 :: Int
  , i32 :: Int
  , i33 :: Int
  , t00 :: T.Text
  , t01 :: T.Text
  , t02 :: T.Text
  , t03 :: T.Text
  , t04 :: T.Text
  , t05 :: T.Text
  , t06 :: T.Text
  , t07 :: T.Text
  , t08 :: T.Text
  , t09 :: T.Text
  , t10 :: T.Text
  , t11 :: T.Text
  , t12 :: T.Text
  , t13 :: T.Text
  , t14 :: T.Text
  , t15 :: T.Text
  , t16 :: T.Text
  , t17 :: T.Text
  , t18 :: T.Text
  , t19 :: T.Text
  , t20 :: T.Text
  , t21 :: T.Text
  , t22 :: T.Text
  , t23 :: T.Text
  , t24 :: T.Text
  , t25 :: T.Text
  , t26 :: T.Text
  , t27 :: T.Text
  , t28 :: T.Text
  , t29 :: T.Text
  , t30 :: T.Text
  , t31 :: T.Text
  , t32 :: T.Text
  , m00 :: Maybe Int
  , m01 :: Maybe Int
  , m02 :: Maybe Int
  , m03 :: Maybe Int
  , m04 :: Maybe Int
  , m05 :: Maybe Int
  , m06 :: Maybe Int
  , m07 :: Maybe Int
  , m08 :: Maybe Int
  , m09 :: Maybe Int
  , m10 :: Maybe Int
  , m11 :: Maybe Int
  , m12 :: Maybe Int
  , m13 :: Maybe Int
  , m14 :: Maybe Int
  , m15 :: Maybe Int
  , m16 :: Maybe Int
  , m17 :: Maybe Int
  , m18 :: Maybe Int
  , m19 :: Maybe Int
  , m20 :: Maybe Int
  , m21 :: Maybe Int
  , m22 :: Maybe Int
  , m23 :: Maybe Int
  , m24 :: Maybe Int
  , m25 :: Maybe Int
  , m26 :: Maybe Int
  , m27 :: Maybe Int
  , m28 :: Maybe Int
  , m29 :: Maybe Int
  , m30 :: Maybe Int
  , m31 :: Maybe Int
  , m32 :: Maybe Int
  }
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Wide

newtype Name = Name T.Text
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Name

newtype Box a = Box {item :: a}
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml (Box a)

newtype Velocity = Velocity Speed
  deriving stock (Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Velocity

instance GenericYamlOptions Velocity where
  type SumEncoding Velocity = TaggedFlat
  yamlOptions = defaultYamlOptions {tagSingleConstructors = True}

newtype Distance = Distance {distance :: Maybe Int}
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Distance

newtype Speed = Speed {speed :: Int}
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Speed

data Config = Config {paths :: [T.Text], jobs :: Int, verbose :: Maybe Bool}
  deriving stock (Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Config

-- The option is on, so that the check of the encoder covers the default too.
-- The encoder uses the default only with 'omitNullFields', to decide if it
-- can leave out a null field.
instance GenericYamlOptions Config where
  yamlOptions = defaultYamlOptions {omitNullFields = True}
  yamlDefault = Just (Config ["."] 1 Nothing)

-- Without 'omitNullFields', the encoder does not use the default.
data Preset = Preset {paths :: [T.Text], jobs :: Int, verbose :: Maybe Bool}
  deriving stock (Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Preset

instance GenericYamlOptions Preset where
  yamlDefault = Just (Preset ["."] 1 Nothing)

encodeServer :: Server -> Node
encodeServer = toYaml

decodeServer :: Node -> Either (NE.NonEmpty (Offset, String)) Server
decodeServer = runParser parseYaml

encodeWide :: Wide -> Node
encodeWide = toYaml

decodeWide :: Node -> Either (NE.NonEmpty (Offset, String)) Wide
decodeWide = runParser parseYaml

encodeName :: Name -> Node
encodeName = toYaml

decodeName :: Node -> Either (NE.NonEmpty (Offset, String)) Name
decodeName = runParser parseYaml

encodeBox :: Box Int -> Node
encodeBox = toYaml

decodeBox :: Node -> Either (NE.NonEmpty (Offset, String)) (Box Int)
decodeBox = runParser parseYaml

encodeVelocity :: Velocity -> Node
encodeVelocity = toYaml

decodeVelocity :: Node -> Either (NE.NonEmpty (Offset, String)) Velocity
decodeVelocity = runParser parseYaml

encodeConfig :: Config -> Node
encodeConfig = toYaml

encodePreset :: Preset -> Node
encodePreset = toYaml

decodePreset :: Node -> Either (NE.NonEmpty (Offset, String)) Preset
decodePreset = runParser parseYaml

decodeConfig :: Node -> Either (NE.NonEmpty (Offset, String)) Config
decodeConfig = runParser parseYaml

----------------------------------------
-- Sums

-- The encoder of Step keeps the representation. The optimizer moves the node
-- of Halt, which has no fields, to the top level. Then the code of the last
-- constructors is in a function with two callers, which takes their
-- representation.

data Turn = TurnLeft | TurnRight | TurnBack
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Turn

data Shape = Circle {radius :: Double} | Dot | Square {side :: Double, angle :: Double}
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Shape

data Step = Ahead Distance | Accelerate Speed | Halt
  deriving stock (Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Step

instance GenericYamlOptions Step where
  type SumEncoding Step = TaggedFlat
  yamlOptions = defaultYamlOptions {tagKey = "step"}

data Figure = Round {radius :: Double} | Named T.Text | Point
  deriving stock (Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Figure

instance GenericYamlOptions Figure where
  type SumEncoding Figure = SingleField

encodeTurn :: Turn -> Node
encodeTurn = toYaml

decodeTurn :: Node -> Either (NE.NonEmpty (Offset, String)) Turn
decodeTurn = runParser parseYaml

encodeShape :: Shape -> Node
encodeShape = toYaml

decodeShape :: Node -> Either (NE.NonEmpty (Offset, String)) Shape
decodeShape = runParser parseYaml

decodeStep :: Node -> Either (NE.NonEmpty (Offset, String)) Step
decodeStep = runParser parseYaml

encodeFigure :: Figure -> Node
encodeFigure = toYaml

decodeFigure :: Node -> Either (NE.NonEmpty (Offset, String)) Figure
decodeFigure = runParser parseYaml

----------------------------------------
-- Lists and fields

-- The decoders of lists have no test. GHC 9.14 does not inline the list
-- method of the instance for GenericYaml, so the derived method passes it the
-- dictionaries of the representation. The method ignores them, and the list
-- decoders are as fast as the written ones.

encodeServers :: [Server] -> Node
encodeServers = toYamlList

encodeServerField :: Node -> Server -> (Node, Node)
encodeServerField = toYamlField

encodeShapes :: [Shape] -> Node
encodeShapes = toYamlList

encodeShapeField :: Node -> Shape -> (Node, Node)
encodeShapeField = toYamlField

decodeServerField :: Node -> Node -> Either (NE.NonEmpty (Offset, String)) Server
decodeServerField k = runParser (parseYamlField k)

decodeShapeField :: Node -> Node -> Either (NE.NonEmpty (Offset, String)) Shape
decodeShapeField k = runParser (parseYamlField k)
