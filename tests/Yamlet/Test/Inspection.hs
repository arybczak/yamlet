{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}

-- | The derived instances contain no generic representation.
--
-- The module exports every binding, because GHC 9.14 removes an unused
-- binding before the plugin checks it.
module Yamlet.Test.Inspection where

import Data.Text qualified as T
import Test.Inspection
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet
import Yamlet.Test.Inspection.Obligations

-- Each type has a test for each method, which checks both obligations. The
-- encoders of Step keep the representation with every GHC, and those of
-- Shape with GHC before 9.12, which 'assertFailureIf' expects. The encoders
-- of lists and fields keep it because they call the encoder of the type. The
-- optimizer moves the node of the constructor without fields, Halt or Dot, to
-- the top level. Then the code of the last constructors is in a function with
-- two callers, which takes their representation.
inspectionTests :: TestTree
inspectionTests =
  testGroup
    "inspection"
    [ testGroup
        "Server"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeServer)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeServer)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeServer)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeServer)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeServerList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeServerList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeServerList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeServerList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeServerField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeServerField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeServerField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeServerField)
        ]
    , testGroup
        "Wide"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeWide)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeWide)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeWide)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeWide)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeWideList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeWideList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeWideList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeWideList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeWideField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeWideField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeWideField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeWideField)
        ]
    , testGroup
        "Name"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeName)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeName)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeName)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeName)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeNameList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeNameList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeNameList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeNameList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeNameField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeNameField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeNameField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeNameField)
        ]
    , testGroup
        "Box"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeBox)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeBox)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeBox)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeBox)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeBoxList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeBoxList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeBoxList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeBoxList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeBoxField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeBoxField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeBoxField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeBoxField)
        ]
    , testGroup
        "Velocity"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeVelocity)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeVelocity)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeVelocity)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeVelocity)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeVelocityList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeVelocityList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeVelocityList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeVelocityList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeVelocityField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeVelocityField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeVelocityField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeVelocityField)
        ]
    , testGroup
        "Distance"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeDistance)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeDistance)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeDistance)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeDistance)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeDistanceList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeDistanceList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeDistanceList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeDistanceList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeDistanceField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeDistanceField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeDistanceField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeDistanceField)
        ]
    , testGroup
        "Speed"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeSpeed)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeSpeed)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeSpeed)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeSpeed)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeSpeedList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeSpeedList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeSpeedList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeSpeedList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeSpeedField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeSpeedField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeSpeedField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeSpeedField)
        ]
    , testGroup
        "Config"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeConfig)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeConfig)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeConfig)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeConfig)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeConfigList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeConfigList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeConfigList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeConfigList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeConfigField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeConfigField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeConfigField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeConfigField)
        ]
    , testGroup
        "Preset"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodePreset)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodePreset)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodePreset)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodePreset)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodePresetList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodePresetList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodePresetList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodePresetList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodePresetField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodePresetField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodePresetField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodePresetField)
        ]
    , testGroup
        "Turn"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeTurn)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeTurn)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeTurn)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeTurn)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeTurnList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeTurnList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeTurnList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeTurnList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeTurnField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeTurnField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeTurnField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeTurnField)
        ]
    , testGroup
        "Shape"
        [ testCase "encode" $ do
            assertFailureIf
              (ghcVersion < (9, 12))
              $(inspectTest $ hasNoGenericRep 'encodeShape)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeShape)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeShape)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeShape)
        , testCase "encode a list" $ do
            assertFailureIf
              (ghcVersion < (9, 12))
              $(inspectTest $ hasNoGenericRep 'encodeShapeList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeShapeList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeShapeList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeShapeList)
        , testCase "encode a field" $ do
            assertFailureIf
              (ghcVersion < (9, 12))
              $(inspectTest $ hasNoGenericRep 'encodeShapeField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeShapeField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeShapeField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeShapeField)
        ]
    , testGroup
        "Step"
        [ testCase "encode" $ do
            assertFailureIf True $(inspectTest $ hasNoGenericRep 'encodeStep)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeStep)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeStep)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeStep)
        , testCase "encode a list" $ do
            assertFailureIf True $(inspectTest $ hasNoGenericRep 'encodeStepList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeStepList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeStepList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeStepList)
        , testCase "encode a field" $ do
            assertFailureIf True $(inspectTest $ hasNoGenericRep 'encodeStepField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeStepField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeStepField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeStepField)
        ]
    , testGroup
        "Figure"
        [ testCase "encode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeFigure)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeFigure)
        , testCase "decode" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeFigure)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeFigure)
        , testCase "encode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeFigureList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeFigureList)
        , testCase "decode a list" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeFigureList)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'decodeFigureList)
        , testCase "encode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'encodeFigureField)
            assertSuccess $(inspectTest $ hasNoGenericDictionaries 'encodeFigureField)
        , testCase "decode a field" $ do
            assertSuccess $(inspectTest $ hasNoGenericRep 'decodeFigureField)
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
  yamlDefault = Just Config {paths = ["."], jobs = 1, verbose = Nothing}

-- Without 'omitNullFields', the encoder does not use the default.
data Preset = Preset {paths :: [T.Text], jobs :: Int, verbose :: Maybe Bool}
  deriving stock (Generic)
  deriving (FromYaml, ToYaml) via GenericYaml Preset

instance GenericYamlOptions Preset where
  yamlDefault = Just Preset {paths = ["."], jobs = 1, verbose = Nothing}

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
