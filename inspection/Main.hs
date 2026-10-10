{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}

-- | The derived instances contain no generic representation.
--
-- The module exports every binding, because GHC 9.14 removes an unused
-- binding before the plugin checks it.
module Main where

import Data.Text qualified as T
import Test.Inspection
import Test.Tasty
import Test.Tasty.HUnit

import Obligations
import Yamlet

-- Each type has a test for each method. The encoder of Step is a known
-- failure with every GHC, and the encoder of Shape with GHC before 9.12,
-- which 'assertFailureIf' expects. The optimizer moves the node of the
-- constructor without fields, Halt or Dot, to the top level. Then the code of
-- the last constructors is in a function with two callers, which takes their
-- representation.
main :: IO ()
main =
  defaultMain $
    testGroup
      "Inspection"
      [ testGroup
          "Server"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeServer)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeServer)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeServerList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeServerList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeServerField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeServerField)
          ]
      , testGroup
          "Wide"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeWide)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeWide)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeWideList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeWideList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeWideField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeWideField)
          ]
      , testGroup
          "Name"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeName)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeName)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeNameList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeNameList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeNameField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeNameField)
          ]
      , testGroup
          "Box"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeBox)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeBox)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeBoxList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeBoxList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeBoxField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeBoxField)
          ]
      , testGroup
          "Velocity"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeVelocity)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeVelocity)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeVelocityList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeVelocityList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeVelocityField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeVelocityField)
          ]
      , testGroup
          "Distance"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeDistance)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeDistance)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeDistanceList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeDistanceList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeDistanceField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeDistanceField)
          ]
      , testGroup
          "Speed"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeSpeed)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeSpeed)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeSpeedList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeSpeedList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeSpeedField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeSpeedField)
          ]
      , testGroup
          "Config"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeConfig)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeConfig)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeConfigList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeConfigList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeConfigField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeConfigField)
          ]
      , testGroup
          "Preset"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodePreset)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodePreset)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodePresetList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodePresetList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodePresetField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodePresetField)
          ]
      , testGroup
          "Turn"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeTurn)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeTurn)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeTurnList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeTurnList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeTurnField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeTurnField)
          ]
      , testGroup
          "Shape"
          [ testCase "encode" $
              assertFailureIf
                (ghcVersion < (9, 12))
                $(inspectTest $ hasNoGenericRep 'encodeShape)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeShape)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeShapeList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeShapeList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeShapeField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeShapeField)
          ]
      , testGroup
          "Step"
          [ testCase "encode" $
              assertFailureIf True $(inspectTest $ hasNoGenericRep 'encodeStep)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeStep)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeStepList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeStepList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeStepField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeStepField)
          ]
      , testGroup
          "Figure"
          [ testCase "encode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'encodeFigure)
          , testCase "decode" $
              assertSuccess $(inspectTest $ hasNoGenericRep 'decodeFigure)
          , testCase "encode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeFigureList)
          , testCase "decode a list" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeFigureList)
          , testCase "encode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeFigureField)
          , testCase "decode a field" $
              assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeFigureField)
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

----------------------------------------
-- Sums

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

----------------------------------------
-- Functions under test

encodeServer :: Server -> Node
encodeServer = toYaml

decodeServer :: Node -> Parser Server
decodeServer = parseYaml

encodeServerList :: [Server] -> Node
encodeServerList = toYamlList

decodeServerList :: Node -> Parser [Server]
decodeServerList = parseYamlList

encodeServerField :: Node -> Server -> (Node, Node)
encodeServerField = toYamlField

decodeServerField :: Node -> Node -> Parser Server
decodeServerField = parseYamlField

encodeWide :: Wide -> Node
encodeWide = toYaml

decodeWide :: Node -> Parser Wide
decodeWide = parseYaml

encodeWideList :: [Wide] -> Node
encodeWideList = toYamlList

decodeWideList :: Node -> Parser [Wide]
decodeWideList = parseYamlList

encodeWideField :: Node -> Wide -> (Node, Node)
encodeWideField = toYamlField

decodeWideField :: Node -> Node -> Parser Wide
decodeWideField = parseYamlField

encodeName :: Name -> Node
encodeName = toYaml

decodeName :: Node -> Parser Name
decodeName = parseYaml

encodeNameList :: [Name] -> Node
encodeNameList = toYamlList

decodeNameList :: Node -> Parser [Name]
decodeNameList = parseYamlList

encodeNameField :: Node -> Name -> (Node, Node)
encodeNameField = toYamlField

decodeNameField :: Node -> Node -> Parser Name
decodeNameField = parseYamlField

encodeBox :: Box Int -> Node
encodeBox = toYaml

decodeBox :: Node -> Parser (Box Int)
decodeBox = parseYaml

encodeBoxList :: [Box Int] -> Node
encodeBoxList = toYamlList

decodeBoxList :: Node -> Parser [Box Int]
decodeBoxList = parseYamlList

encodeBoxField :: Node -> Box Int -> (Node, Node)
encodeBoxField = toYamlField

decodeBoxField :: Node -> Node -> Parser (Box Int)
decodeBoxField = parseYamlField

encodeVelocity :: Velocity -> Node
encodeVelocity = toYaml

decodeVelocity :: Node -> Parser Velocity
decodeVelocity = parseYaml

encodeVelocityList :: [Velocity] -> Node
encodeVelocityList = toYamlList

decodeVelocityList :: Node -> Parser [Velocity]
decodeVelocityList = parseYamlList

encodeVelocityField :: Node -> Velocity -> (Node, Node)
encodeVelocityField = toYamlField

decodeVelocityField :: Node -> Node -> Parser Velocity
decodeVelocityField = parseYamlField

encodeDistance :: Distance -> Node
encodeDistance = toYaml

decodeDistance :: Node -> Parser Distance
decodeDistance = parseYaml

encodeDistanceList :: [Distance] -> Node
encodeDistanceList = toYamlList

decodeDistanceList :: Node -> Parser [Distance]
decodeDistanceList = parseYamlList

encodeDistanceField :: Node -> Distance -> (Node, Node)
encodeDistanceField = toYamlField

decodeDistanceField :: Node -> Node -> Parser Distance
decodeDistanceField = parseYamlField

encodeSpeed :: Speed -> Node
encodeSpeed = toYaml

decodeSpeed :: Node -> Parser Speed
decodeSpeed = parseYaml

encodeSpeedList :: [Speed] -> Node
encodeSpeedList = toYamlList

decodeSpeedList :: Node -> Parser [Speed]
decodeSpeedList = parseYamlList

encodeSpeedField :: Node -> Speed -> (Node, Node)
encodeSpeedField = toYamlField

decodeSpeedField :: Node -> Node -> Parser Speed
decodeSpeedField = parseYamlField

encodeConfig :: Config -> Node
encodeConfig = toYaml

decodeConfig :: Node -> Parser Config
decodeConfig = parseYaml

encodeConfigList :: [Config] -> Node
encodeConfigList = toYamlList

decodeConfigList :: Node -> Parser [Config]
decodeConfigList = parseYamlList

encodeConfigField :: Node -> Config -> (Node, Node)
encodeConfigField = toYamlField

decodeConfigField :: Node -> Node -> Parser Config
decodeConfigField = parseYamlField

encodePreset :: Preset -> Node
encodePreset = toYaml

decodePreset :: Node -> Parser Preset
decodePreset = parseYaml

encodePresetList :: [Preset] -> Node
encodePresetList = toYamlList

decodePresetList :: Node -> Parser [Preset]
decodePresetList = parseYamlList

encodePresetField :: Node -> Preset -> (Node, Node)
encodePresetField = toYamlField

decodePresetField :: Node -> Node -> Parser Preset
decodePresetField = parseYamlField

encodeTurn :: Turn -> Node
encodeTurn = toYaml

decodeTurn :: Node -> Parser Turn
decodeTurn = parseYaml

encodeTurnList :: [Turn] -> Node
encodeTurnList = toYamlList

decodeTurnList :: Node -> Parser [Turn]
decodeTurnList = parseYamlList

encodeTurnField :: Node -> Turn -> (Node, Node)
encodeTurnField = toYamlField

decodeTurnField :: Node -> Node -> Parser Turn
decodeTurnField = parseYamlField

encodeShape :: Shape -> Node
encodeShape = toYaml

decodeShape :: Node -> Parser Shape
decodeShape = parseYaml

encodeShapeList :: [Shape] -> Node
encodeShapeList = toYamlList

decodeShapeList :: Node -> Parser [Shape]
decodeShapeList = parseYamlList

encodeShapeField :: Node -> Shape -> (Node, Node)
encodeShapeField = toYamlField

decodeShapeField :: Node -> Node -> Parser Shape
decodeShapeField = parseYamlField

encodeStep :: Step -> Node
encodeStep = toYaml

decodeStep :: Node -> Parser Step
decodeStep = parseYaml

encodeStepList :: [Step] -> Node
encodeStepList = toYamlList

decodeStepList :: Node -> Parser [Step]
decodeStepList = parseYamlList

encodeStepField :: Node -> Step -> (Node, Node)
encodeStepField = toYamlField

decodeStepField :: Node -> Node -> Parser Step
decodeStepField = parseYamlField

encodeFigure :: Figure -> Node
encodeFigure = toYaml

decodeFigure :: Node -> Parser Figure
decodeFigure = parseYaml

encodeFigureList :: [Figure] -> Node
encodeFigureList = toYamlList

decodeFigureList :: Node -> Parser [Figure]
decodeFigureList = parseYamlList

encodeFigureField :: Node -> Figure -> (Node, Node)
encodeFigureField = toYamlField

decodeFigureField :: Node -> Node -> Parser Figure
decodeFigureField = parseYamlField
