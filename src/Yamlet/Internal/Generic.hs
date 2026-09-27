{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The options of the generic instances, and the parts of the generic
-- representation that the encoder and the decoder share.
module Yamlet.Internal.Generic
  ( -- * Options
    YamlOptions (..)
  , defaultYamlOptions
  , SumEncodingKind (..)
  , GenericYaml (..)
  , snakeCase
  , kebabCase

    -- * Constructors
  , GConstructors (..)
  , GEncoding (..)
  , isEnum
  , isTagged
  , constructorTag

    -- * Fields
  , GFields (..)
  , fieldKey
  ) where

import Data.Char
import Data.Kind
import Data.Proxy
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits

----------------------------------------
-- Options

-- | How a type is encoded and decoded.
--
-- The instances do not check the options. Options that give two keys of a
-- mapping or two constructors the same text encode values that do not read
-- back, as the fields below describe.
data YamlOptions = YamlOptions
  { fieldLabelModifier :: String -> String
  -- ^ The key of a field from the name of the field. If two fields of a
  -- constructor get the same key, e.g. @fooBar@ and @foo_bar@ with
  -- 'snakeCase', the constructor encodes as a mapping with two equal keys,
  -- which does not read back.
  , constructorTagModifier :: String -> String
  -- ^ The tag of a constructor from the name of the constructor. If two
  -- constructors get the same tag, e.g. @FooBar@ and @Foo_bar@ with
  -- 'snakeCase', the decoder reads the tag as the first of them.
  , tagKey :: T.Text
  -- ^ The key of the tag, @tag@ by default. A record with a field of the same
  -- key encodes as a mapping with two equal keys, which does not read back.
  , contentsKey :: T.Text
  -- ^ The key of the fields of a tagged constructor without field names,
  -- @contents@ by default. If it is the same as 'tagKey', such a constructor
  -- encodes as a mapping with two equal keys, which does not read back.
  , tagSingleConstructors :: Bool
  -- ^ Give a type with one constructor a tag too. Off by default.
  , allNullaryToStringTag :: Bool
  -- ^ Encode a type whose constructors have no fields as a string. On by
  -- default.
  , omitNullFields :: Bool
  -- ^ Leave out a field whose value is null, e.g. 'Nothing'. Off by default.
  -- With 'yamlDefault', a null field stays if its default is not null,
  -- because the decoder would fill the missing key from the default.
  , rejectUnknownFields :: Bool
  -- ^ Reject a key that is no field of the constructor. Off by default.
  }
  deriving stock (Generic)

defaultYamlOptions :: YamlOptions
defaultYamlOptions =
  YamlOptions
    { fieldLabelModifier = id
    , constructorTagModifier = id
    , tagKey = "tag"
    , contentsKey = "contents"
    , tagSingleConstructors = False
    , allNullaryToStringTag = True
    , omitNullFields = False
    , rejectUnknownFields = False
    }

-- | How a tagged constructor goes in a mapping. The choice is a type, see
-- 'SumEncoding', because it changes the shapes of the constructors that a
-- type can have.
data SumEncodingKind
  = -- | The fields go next to the tag, e.g. @{tag: Circle, radius: 1}@, and a
    -- field without a name goes under the contents key, e.g.
    -- @{tag: Forward, contents: 10}@.
    TaggedObject
  | -- | The entries of a field without a name go next to the tag, e.g.
    -- @{tag: Ahead, distance: 10}@ for @Ahead (Distance 10)@. The
    -- constructors must have one field without a name or no fields,
    -- otherwise the type is a type error.
    --
    -- The field must encode as a mapping with a key, and no key can be the
    -- tag key. Otherwise the constructor encodes as with 'TaggedObject'.
    -- Thus the field of a type with the same tag key stays under the
    -- contents key. An enumeration uses the form if
    -- 'Yamlet.Generic.allNullaryToStringTag' is off.
    --
    -- The keys of the mapping belong to the field, so the options of its
    -- type apply to them, e.g. 'Yamlet.Generic.rejectUnknownFields'.
    TaggedFlat
  | -- | A mapping with one key, the tag, and the fields as its value, e.g.
    -- @{Circle: {radius: 1}}@. A field without a name is the value, e.g.
    -- @{Forward: 10}@, and a constructor without fields is its tag, e.g.
    -- @Dot@. The tag key and the contents key play no part.
    --
    -- Each constructor has its own value, so the constructors of a type can
    -- mix named fields with a field without a name. A second key in the
    -- mapping is an error. 'Yamlet.Generic.rejectUnknownFields' applies to
    -- the named fields in the value.
    --
    -- The key of a constructor with named fields is no field, so its
    -- comments are lost. The key of a field without a name goes to the
    -- field, e.g. for a 'Yamlet.Commented' value.
    SingleField
  deriving stock (Eq, Show)

-- | The configuration of the generic instances of t'Yamlet.Decode.FromYaml'
-- and t'Yamlet.Encode.ToYaml' for a type: the options and the default value.
class GenericYaml a where
  -- | How a tagged constructor goes in a mapping, 'TaggedObject' by default.
  type SumEncoding a :: SumEncodingKind

  type SumEncoding a = TaggedObject

  yamlOptions :: YamlOptions
  yamlOptions = defaultYamlOptions

  -- | The value that gives the fields of missing keys, e.g. the default
  -- configuration. Without it, a missing key decodes like null. A key with
  -- the value null is not missing. For a sum type, the default applies only
  -- to its own constructor.
  yamlDefault :: Maybe a
  yamlDefault = Nothing

-- | The words of a name in lower case, separated by underscores, e.g.
-- @source_paths@ for @sourcePaths@ or @SourcePaths@, and @http_server@ for
-- @HTTPServer@. The rules are the same as for @camelTo2 \'_\'@ of aeson.
snakeCase :: String -> String
snakeCase = separateWords '_'

-- | Like 'snakeCase', but with hyphens, e.g. @source-paths@ for
-- @sourcePaths@.
kebabCase :: String -> String
kebabCase = separateWords '-'

-- A word starts at an upper-case letter after a lower-case one, and at the
-- last letter of an acronym before a lower-case one.
separateWords :: Char -> String -> String
separateWords sep = map toLower . afterLower . beforeLower
  where
    beforeLower :: String -> String
    beforeLower = \case
      x : u : l : rest | isUpper u && isLower l -> x : sep : u : l : beforeLower rest
      x : rest -> x : beforeLower rest
      [] -> []

    afterLower :: String -> String
    afterLower = \case
      l : u : rest | isLower l && isUpper u -> l : sep : u : afterLower rest
      x : rest -> x : afterLower rest
      [] -> []

----------------------------------------
-- Constructors

class GConstructors f where
  gConstructorNames :: [String]

  gConstructorCount :: Int

  -- | No constructor has fields.
  gNullary :: Bool

instance (GConstructors f, GConstructors g) => GConstructors (f :+: g) where
  gConstructorNames = gConstructorNames @f ++ gConstructorNames @g
  gConstructorCount = gConstructorCount @f + gConstructorCount @g
  gNullary = gNullary @f && gNullary @g

instance GConstructors V1 where
  gConstructorNames = []
  gConstructorCount = 0
  gNullary = True

-- | The error for a type without constructors, whose representation is 'V1'.
type NoConstructors = Text "A type without constructors cannot derive FromYaml or ToYaml"

instance (KnownSymbol name, GFields f) => GConstructors (C1 (MetaCons name fixity isRecord) f) where
  gConstructorNames = [symbolVal (Proxy @name)]
  gConstructorCount = 1
  gNullary = gArity @f == 0

-- | The type has only constructors without fields, and the options encode it
-- as a string.
isEnum :: forall f. GConstructors f => YamlOptions -> Bool
isEnum opts = opts.allNullaryToStringTag && gNullary @f

-- | The value of 'SumEncoding', if the constructors allow it. The instances
-- also check the shape of the constructors, because every derived instance
-- needs this class.
class GEncoding (e :: SumEncodingKind) f where
  gEncoding :: SumEncodingKind

instance ValidShape (GShape f) => GEncoding TaggedObject f where
  gEncoding = validShape @(GShape f) `seq` TaggedObject

instance ValidShape (FlatShape (GShape f)) => GEncoding TaggedFlat f where
  gEncoding = validShape @(FlatShape (GShape f)) `seq` TaggedFlat

instance ValidShape (SingleShape f) => GEncoding SingleField f where
  gEncoding = validShape @(SingleShape f) `seq` SingleField

-- | The fields of the constructors of a type. A constructor without fields
-- fits with both kinds of fields.
data Shape
  = NoFields
  | -- | One field without a name, in the constructor with the name.
    UnnamedField Symbol
  | -- | Named fields, in the constructor with the name.
    NamedFields Symbol

-- | The shape of the constructors. A constructor with several fields without
-- names, and a type that mixes named fields with a field without a name, are
-- type errors.
type family GShape (f :: Type -> Type) :: Shape where
  GShape (f :+: g) = CombineShapes (GShape f) (GShape g)
  GShape (C1 (MetaCons name fixity True) f) = NamedFields name
  GShape (C1 (MetaCons name fixity False) U1) = NoFields
  GShape (C1 (MetaCons name fixity False) (S1 m f)) = UnnamedField name
  GShape (C1 (MetaCons name fixity False) (f :*: g)) =
    TypeError
      ( Text "The constructor "
          :<>: Text name
          :<>: Text " has several fields without names."
          :$$: Text "Give the fields names, or use a tuple."
      )
  GShape V1 = TypeError NoConstructors

type family CombineShapes (a :: Shape) (b :: Shape) :: Shape where
  CombineShapes NoFields b = b
  CombineShapes a NoFields = a
  CombineShapes (NamedFields a) (NamedFields _) = NamedFields a
  CombineShapes (UnnamedField a) (UnnamedField _) = UnnamedField a
  CombineShapes (NamedFields a) (UnnamedField b) = MixedFields a b
  CombineShapes (UnnamedField b) (NamedFields a) = MixedFields a b

type family MixedFields (named :: Symbol) (unnamed :: Symbol) :: Shape where
  MixedFields named unnamed =
    TypeError
      ( Text "The constructor "
          :<>: Text named
          :<>: Text " has named fields and the constructor "
          :<>: Text unnamed
          :<>: Text " has one field without a name."
          :$$: Text "The constructors of a type must all have named fields or all have one field without a name."
      )

-- | The shape is valid. The instances match on the shape, so that GHC
-- reduces it and reports its type errors. With deferred type errors, e.g. in
-- a test of the errors, the method throws the error at run time.
class ValidShape (s :: Shape) where
  validShape :: ()

instance ValidShape NoFields where validShape = ()
instance ValidShape (UnnamedField name) where validShape = ()
instance ValidShape (NamedFields name) where validShape = ()

-- | A shape of 'SingleField', which checks each constructor as 'GShape' does,
-- but lets the constructors mix their fields. The equations match both
-- shapes, so that GHC reduces both and reports their type errors.
type family SingleShape (f :: Type -> Type) :: Shape where
  SingleShape (f :+: g) = EitherShape (SingleShape f) (SingleShape g)
  SingleShape f = GShape f

type family EitherShape (a :: Shape) (b :: Shape) :: Shape where
  EitherShape NoFields b = b
  EitherShape a NoFields = a
  EitherShape (NamedFields a) (NamedFields _) = NamedFields a
  EitherShape (NamedFields a) (UnnamedField _) = NamedFields a
  EitherShape (UnnamedField a) (NamedFields _) = UnnamedField a
  EitherShape (UnnamedField a) (UnnamedField _) = UnnamedField a

-- | The shape, if 'TaggedFlat' has fields to flatten in it.
type family FlatShape (s :: Shape) :: Shape where
  FlatShape (NamedFields name) =
    TypeError
      ( Text "TaggedFlat needs constructors with one field without a name, but the constructor "
          :<>: Text name
          :<>: Text " has named fields."
      )
  FlatShape s = s

isTagged :: forall f. GConstructors f => YamlOptions -> Bool
isTagged opts = opts.tagSingleConstructors || gConstructorCount @f > 1

constructorTag :: YamlOptions -> String -> T.Text
constructorTag opts = T.pack . opts.constructorTagModifier

----------------------------------------
-- Fields

class GFields f where
  -- | The fields have names.
  gNamed :: Bool

  gArity :: Int

  -- | The keys of the fields.
  gNames :: YamlOptions -> [T.Text]

instance GFields U1 where
  gNamed = False
  gArity = 0
  gNames _ = []

instance (GFields f, GFields g) => GFields (f :*: g) where
  gNamed = gNamed @f
  gArity = gArity @f + gArity @g
  gNames opts = gNames @f opts ++ gNames @g opts

instance KnownSymbol name => GFields (S1 (MetaSel (Just name) u s d) f) where
  gNamed = True
  gArity = 1
  gNames opts = [fieldKey @name opts]

instance GFields (S1 (MetaSel Nothing u s d) f) where
  gNamed = False
  gArity = 1
  gNames _ = []

fieldKey :: forall name. KnownSymbol name => YamlOptions -> T.Text
fieldKey opts = T.pack (opts.fieldLabelModifier (symbolVal (Proxy @name)))
