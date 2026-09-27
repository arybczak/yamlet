{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}

-- | The derived instances contain no generic representation.
--
-- The module exports every binding, because GHC 9.14 removes an unused
-- binding before the plugin checks it.
module Main where

import Data.Text qualified as T
import GHC.Generics (Generic)
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
          [ -- GHC 9.4 shares the code of the last constructors in a join point,
            -- as for other sums. The arguments are constants, so each one
            -- runs once.
            testCase "encode Turn" $ assertFailureIf (ghcVersion == (9, 4)) $(inspectTest $ hasNoGenericRep 'encodeTurn)
          , testCase "decode Turn" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeTurn)
          , testCase "decode Shape" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeShape)
          , testCase "decode Step" $ assertSuccess $(inspectTest $ hasNoGenericRep 'decodeStep)
          ]
      ]

----------------------------------------
-- Products

data Server = Server {host :: T.Text, port :: Int, tags :: Maybe [T.Text]}
  deriving stock (Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Wide = Wide
  { i00, i01, i02, i03, i04, i05, i06, i07, i08, i09, i10, i11, i12, i13, i14,
    i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29,
    i30, i31, i32, i33 :: Int
  , t00, t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14,
    t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29,
    t30, t31, t32 :: T.Text
  , m00, m01, m02, m03, m04, m05, m06, m07, m08, m09, m10, m11, m12, m13, m14,
    m15, m16, m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29,
    m30, m31, m32 :: Maybe Int
  }
  deriving stock (Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

newtype Name = Name T.Text
  deriving stock (Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

newtype Box a = Box {item :: a}
  deriving stock (Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Velocity = Velocity Speed
  deriving stock (Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Velocity where
  type FlattenFields Velocity = True
  yamlOptions = defaultYamlOptions {tagSingleConstructors = True}

newtype Distance = Distance {distance :: Maybe Int}
  deriving stock (Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

newtype Speed = Speed {speed :: Int}
  deriving stock (Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Config = Config {paths :: [T.Text], jobs :: Int, verbose :: Maybe Bool}
  deriving stock (Generic)
  deriving anyclass (FromYaml, ToYaml)

-- The encoder uses the default only with 'omitNullFields', to decide if it
-- can leave out a null field. So the option makes the check of the encoder
-- cover the default too.
instance GenericYaml Config where
  yamlOptions = defaultYamlOptions {omitNullFields = True}
  yamlDefault = Just (Config ["."] 1 Nothing)

-- Without 'omitNullFields', the encoder does not use the default.
data Preset = Preset {paths :: [T.Text], jobs :: Int, verbose :: Maybe Bool}
  deriving stock (Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Preset where
  yamlDefault = Just (Preset ["."] 1 Nothing)

encodeServer :: Server -> Node
encodeServer = toYaml

decodeServer :: Node -> Either (Offset, String) Server
decodeServer = runParser parseYaml

encodeWide :: Wide -> Node
encodeWide = toYaml

decodeWide :: Node -> Either (Offset, String) Wide
decodeWide = runParser parseYaml

encodeName :: Name -> Node
encodeName = toYaml

decodeName :: Node -> Either (Offset, String) Name
decodeName = runParser parseYaml

encodeBox :: Box Int -> Node
encodeBox = toYaml

decodeBox :: Node -> Either (Offset, String) (Box Int)
decodeBox = runParser parseYaml

encodeVelocity :: Velocity -> Node
encodeVelocity = toYaml

decodeVelocity :: Node -> Either (Offset, String) Velocity
decodeVelocity = runParser parseYaml

encodeConfig :: Config -> Node
encodeConfig = toYaml

encodePreset :: Preset -> Node
encodePreset = toYaml

decodePreset :: Node -> Either (Offset, String) Preset
decodePreset = runParser parseYaml

decodeConfig :: Node -> Either (Offset, String) Config
decodeConfig = runParser parseYaml

----------------------------------------
-- Sums

-- The encoder of a sum type with fields keeps the representation. The
-- optimizer shares the code of the last constructors in one join point,
-- which takes their representation, even with three constructors.

data Turn = TurnLeft | TurnRight | TurnBack
  deriving stock (Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Shape = Circle {radius :: Double} | Dot | Square {side :: Double, angle :: Double}
  deriving stock (Generic)
  deriving anyclass (GenericYaml, FromYaml, ToYaml)

data Step = Ahead Distance | Accelerate Speed | Halt
  deriving stock (Generic)
  deriving anyclass (FromYaml, ToYaml)

instance GenericYaml Step where
  type FlattenFields Step = True
  yamlOptions = defaultYamlOptions {tagKey = "step"}

encodeTurn :: Turn -> Node
encodeTurn = toYaml

decodeTurn :: Node -> Either (Offset, String) Turn
decodeTurn = runParser parseYaml

decodeShape :: Node -> Either (Offset, String) Shape
decodeShape = runParser parseYaml

decodeStep :: Node -> Either (Offset, String) Step
decodeStep = runParser parseYaml
