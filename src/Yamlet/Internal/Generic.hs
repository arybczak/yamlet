{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The options of the generic instances, and the parts of the generic
-- representation that the encoder and the decoder share.
module Yamlet.Internal.Generic
  ( -- * Options
    YamlOptions (..)
  , defaultYamlOptions
  , GenericYaml (..)
  , snakeCase
  , kebabCase

    -- * Constructors
  , GConstructors (..)
  , GFlatten (..)
  , NoConstructors
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
data YamlOptions = YamlOptions
  { fieldLabelModifier :: String -> String
  -- ^ The key of a field from the name of the field.
  , constructorTagModifier :: String -> String
  -- ^ The tag of a constructor from the name of the constructor.
  , tagKey :: T.Text
  -- ^ The key of the tag, @tag@ by default. A record with a field of the same
  -- key encodes as a mapping with two equal keys, which does not read back.
  , contentsKey :: T.Text
  -- ^ The key of the fields of a tagged constructor without field names,
  -- @contents@ by default.
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

-- | The configuration of the generic instances of 'Yamlet.Decode.FromYaml'
-- and 'Yamlet.Encode.ToYaml' for a type: the options and the default value.
class GenericYaml a where
  -- | Put the entries of the field of a tagged constructor without field
  -- names in the mapping of the constructor, next to the tag. 'False' by
  -- default. A constructor with several fields without names is a type
  -- error.
  --
  -- The field must encode as a mapping with a key, and no key can be the tag
  -- key. Otherwise the constructor encodes as without the option. Thus the
  -- field of a type with the same tag key stays under the contents key. An
  -- enumeration merges if 'Yamlet.Generic.allNullaryToStringTag' is off.
  --
  -- The keys of the mapping belong to the field, so the options of its type
  -- apply to them, e.g. 'Yamlet.Generic.rejectUnknownFields'.
  type FlattenFields a :: Bool

  type FlattenFields a = False

  yamlOptions :: YamlOptions
  yamlOptions = defaultYamlOptions

  -- | The value that gives the fields of missing keys, e.g. the default
  -- configuration. Without it, a missing key decodes like null. For a sum
  -- type, the default applies only to its own constructor.
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

instance TypeError NoConstructors => GConstructors V1 where
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

-- | The value of 'FlattenFields', if the constructors allow it.
class GFlatten (flat :: Bool) f where
  gFlatten :: Bool

instance GFlatten False f where
  gFlatten = False

instance FlatConstructors f => GFlatten True f where
  gFlatten = True

type family FlatConstructors f :: Constraint where
  FlatConstructors (f :+: g) = (FlatConstructors f, FlatConstructors g)
  FlatConstructors (C1 (MetaCons name fixity False) (f :*: g)) =
    TypeError
      ( Text "FlattenFields allows one field without a name, but the constructor "
          :<>: Text name
          :<>: Text " has more"
      )
  FlatConstructors f = ()

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
