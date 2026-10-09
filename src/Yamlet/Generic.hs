{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Instances of t'Yamlet.Decode.FromYaml' and t'Yamlet.Encode.ToYaml' from
-- the t'GHC.Generics.Generic' representation of a type.
--
-- A type derives the instances via t'GenericYaml', and its instance of
-- 'GenericYamlOptions' gives the options:
--
-- >>> :{
-- data Server = Server {host :: T.Text, port :: Int}
--   deriving stock (Generic, Show)
--   deriving anyclass (GenericYamlOptions)
--   deriving (FromYaml, ToYaml) via GenericYaml Server
-- :}
--
-- >>> decodeText @Server "host: localhost\nport: 80\n"
-- Right (Server {host = "localhost", port = 80})
--
-- >>> T.putStr (encodeText (Server "localhost" 80))
-- host: localhost
-- port: 80
--
-- A type with other options defines 'yamlOptions' in its instance of
-- 'GenericYamlOptions':
--
-- >>> :{
-- data Build = Build {sourcePaths :: [T.Text], ghcOptions :: [T.Text]}
--   deriving stock (Generic)
--   deriving (ToYaml) via GenericYaml Build
-- instance GenericYamlOptions Build where
--   yamlOptions = defaultYamlOptions {fieldLabelModifier = snakeCase}
-- :}
--
-- >>> T.putStr (encodeText (Build ["src"] ["-Wall"]))
-- source_paths:
-- - src
-- ghc_options:
-- - -Wall
--
-- = Encoding
--
-- The encoding of a type depends on its constructors and their fields:
--
-- * A record is a mapping of its fields, e.g. @{host: localhost, port: 80}@.
--
-- * A type whose constructors have no fields is a string with the name of
--   the constructor, e.g. @TurnLeft@. This includes a type with one such
--   constructor, which aeson writes as an empty list.
--
-- * A type with several constructors is a mapping with the name of the
--   constructor under the tag key, next to the fields of the constructor,
--   e.g. @{tag: Circle, radius: 1}@. A constructor with a field without a
--   name has its field under the contents key, e.g.
--   @{tag: Forward, contents: 10}@.
--
-- * A type with one constructor and a field without a name is its field.
--
-- * With the encoding 'TaggedFlat', the entries of a field without a name go
--   in the mapping of the constructor, e.g. @{tag: Ahead, distance: 10}@ for
--   @Ahead (Distance 10)@.
--
-- * With the encoding 'SingleField', a constructor is a mapping with its
--   name as the only key, e.g. @{Circle: {radius: 1}}@ or @{Forward: 10}@,
--   and a constructor without fields is its name, e.g. @Dot@. aeson writes
--   such a constructor as @{Dot: []}@.
--
-- The default encoding of a sum type is 'TaggedObject':
--
-- >>> :{
-- data Shape = Circle {radius :: Double} | Dot
--   deriving stock (Generic)
--   deriving anyclass (GenericYamlOptions)
--   deriving (ToYaml) via GenericYaml Shape
-- :}
--
-- >>> T.putStr (encodeText [Circle 1, Dot])
-- - tag: Circle
--   radius: 1.0
-- - tag: Dot
--
-- >>> :{
-- data Move = Forward Int | Stop
--   deriving stock (Generic)
--   deriving anyclass (GenericYamlOptions)
--   deriving (ToYaml) via GenericYaml Move
-- :}
--
-- >>> T.putStr (encodeText [Forward 10, Stop])
-- - tag: Forward
--   contents: 10
-- - tag: Stop
--
-- The instance of 'GenericYamlOptions' chooses another encoding:
--
-- >>> :{
-- data Distance = Distance {distance :: Int}
--   deriving stock (Generic)
--   deriving anyclass (GenericYamlOptions)
--   deriving (ToYaml) via GenericYaml Distance
-- :}
--
-- >>> :{
-- data Step = Ahead Distance | Halt
--   deriving stock (Generic)
--   deriving (ToYaml) via GenericYaml Step
-- instance GenericYamlOptions Step where
--   type SumEncoding Step = TaggedFlat
-- :}
--
-- >>> T.putStr (encodeText [Ahead (Distance 10), Halt])
-- - tag: Ahead
--   distance: 10
-- - tag: Halt
--
-- >>> :{
-- data Figure = Round {radius :: Double} | Named T.Text | Point
--   deriving stock (Generic)
--   deriving (ToYaml) via GenericYaml Figure
-- instance GenericYamlOptions Figure where
--   type SumEncoding Figure = SingleField
-- :}
--
-- >>> T.putStr (encodeText [Round 1, Named "x", Point])
-- - Round:
--     radius: 1.0
-- - Named: x
-- - Point
--
-- = Shapes
--
-- Every constructor must have no fields, one field without a name, or named
-- fields. A type with several constructors cannot mix named fields with a
-- field without a name, but a constructor without fields fits with both.
-- 'SingleField' allows the mix, because each constructor has its own value.
-- 'TaggedFlat' needs constructors with one field without a name or no
-- fields.
--
-- Another shape is a compile error that names the constructors, e.g. a
-- constructor with several fields without names. Give such fields names, or
-- put them in a tuple.
--
-- = Missing keys
--
-- A missing field takes its value from the 'yamlDefault' of the type that
-- has the field, if that type has a default. Otherwise the field decodes as
-- if its value is null. A missing contents key does the same. Thus a field
-- of type 'Maybe' is optional, and a missing field of another type is an
-- error, even if the type of the field has its own 'yamlDefault': only the
-- default of the type that has the field counts.
--
-- A type with a default configuration derives the decoder like this:
--
-- >>> :{
-- data Config = Config {name :: T.Text, retries :: Int, proxy :: Maybe T.Text}
--   deriving stock (Generic, Show)
--   deriving (FromYaml) via GenericYaml Config
-- instance GenericYamlOptions Config where
--   yamlDefault = Just (Config "app" 3 (Just "proxy.local"))
-- :}
--
-- >>> decodeText @Config "retries: 5\n"
-- Right (Config {name = "app", retries = 5, proxy = Just "proxy.local"})
--
-- >>> decodeText @Config "proxy: null\n"
-- Right (Config {name = "app", retries = 3, proxy = Nothing})
--
-- A field with the value 'requiredField' has no default:
--
-- >>> :{
-- data Account = Account {user :: T.Text, shell :: T.Text}
--   deriving stock (Generic, Show)
--   deriving (FromYaml) via GenericYaml Account
-- instance GenericYamlOptions Account where
--   yamlDefault = Just (Account requiredField "/bin/sh")
-- :}
--
-- >>> decodeText @Account "user: alice\n"
-- Right (Account {user = "alice", shell = "/bin/sh"})
--
-- >>> either printErrors print (decodeText @Account "shell: /bin/zsh\n")
-- input.yaml:1:1: missing key "user"
--   |
-- 1 | shell: /bin/zsh
--   | ^
--
-- A present key that holds a mapping takes the missing keys of that mapping
-- from the default of its own type, not from the outer default:
--
-- >>> :{
-- data Endpoint = Endpoint {host :: T.Text, port :: Int}
--   deriving stock (Generic, Show)
--   deriving (FromYaml) via GenericYaml Endpoint
-- instance GenericYamlOptions Endpoint where
--   yamlDefault = Just (Endpoint "localhost" 80)
-- :}
--
-- >>> :{
-- data Service = Service {name :: T.Text, endpoint :: Endpoint}
--   deriving stock (Generic, Show)
--   deriving (FromYaml) via GenericYaml Service
-- instance GenericYamlOptions Service where
--   yamlDefault = Just (Service "app" (Endpoint "example.com" 443))
-- :}
--
-- >>> decodeText @Service "endpoint:\n  port: 8080\n"
-- Right (Service {name = "app", endpoint = Endpoint {host = "localhost", port = 8080}})
--
-- >>> decodeText @Service "name: web\n"
-- Right (Service {name = "web", endpoint = Endpoint {host = "example.com", port = 443}})
--
-- With 'TaggedFlat', the keys of a field without a name are next to the tag,
-- but they belong to the field. The missing keys of the field also come
-- from the default of its type. The outer default applies to such a field
-- only if the constructor has no keys besides the tag.
--
-- An explicit null is not a missing key, so it goes to the decoder of the
-- field. E.g. @proxy: null@ gives 'Nothing' for a field of type 'Maybe', and
-- @proxy:@ without a value gives 'Nothing' too. An empty document is also
-- null, so a type with a default does not decode from it.
module Yamlet.Generic
  ( -- * Deriving
    GenericYaml (..)
  , GenericYamlOptions (..)
  , YamlOptions (..)
  , defaultYamlOptions
  , SumEncodingKind (..)
  , requiredField

    -- * Modifiers
  , snakeCase
  , kebabCase

    -- * Instances by hand
  , genericToYaml
  , genericParseYaml

    -- * Classes of the representation
  , GDatatype (Constructors)
  , GConstructors
  , GEncoding
  , GToConstructor
  , GFromConstructor
  , GFields
  , GToFields
  , GFromFields

    -- * Re-exports
  , Generic
  ) where

import Control.Exception hiding (TypeError)
import Control.Monad
import Data.Char
import Data.Coerce
import Data.Kind
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Proxy
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits
import System.IO.Unsafe

import Yamlet.Internal.FromYaml
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Internal.ToYaml
import Yamlet.Internal.Utils
import Yamlet.Internal.View
import Yamlet.Value

----------------------------------------
-- Deriving

-- | A newtype to derive t'Yamlet.Decode.FromYaml' and t'Yamlet.Encode.ToYaml'
-- with @deriving via@. The instances come from the t'GHC.Generics.Generic'
-- representation of the type and its instance of 'GenericYamlOptions'.
newtype GenericYaml a = GenericYaml a

instance
  ( Generic a
  , GenericYamlOptions a
  , GDatatype (Rep a)
  , Constructors (Rep a) ~ f
  , GConstructors f
  , GEncoding (SumEncoding a) f
  , GToConstructor f
  , ToYaml a
  )
  => ToYaml (GenericYaml a)
  where
  toYaml = coerce (genericToYaml @a)
  -- The pragma keeps the source of the method as its unfolding, and GHC
  -- inlines it at the type of the derived instance, together with
  -- 'genericToYaml'. Without it, GHC does not inline the optimized method
  -- there, the derived encoders keep the generic representation, and the
  -- inspection tests of the encoders fail.
  {-# INLINE toYaml #-}

  -- The list and the field encode their values with the instance of
  -- 'ToYaml a', for the reason at 'parseYamlList' below. With the defaults
  -- of the class, the benchmark derive.contents.toYaml.generic is slower.
  toYamlList xs = S.sequenceNode (map (toYaml @a) (coerce xs))

  -- A type that is its field passes the key to the field.
  toYamlField k (GenericYaml x)
    | not (isTagged @f (yamlOptions @a))
    , Just entry <- gToUntaggedEntry k (gUnwrap (from x)) =
        entry
    | otherwise = (k, toYaml @a x)

instance
  ( Generic a
  , GenericYamlOptions a
  , GDatatype (Rep a)
  , Constructors (Rep a) ~ f
  , GConstructors f
  , GEncoding (SumEncoding a) f
  , GFromConstructor f
  , FromYaml a
  )
  => FromYaml (GenericYaml a)
  where
  parseYaml = coerce (genericParseYaml @a)
  -- The pragma has the reason of the one on 'toYaml'. Without it, the
  -- optimized method is too large for an unfolding, and the inspection tests
  -- of the decoders fail.
  {-# INLINE parseYaml #-}

  -- The list and the field decode their values with the instance of
  -- 'FromYaml a', i.e. the derived instance of the type, where GHC inlined
  -- the generic decoder at that type. The defaults of the class would call
  -- 'parseYaml' of this instance instead. GHC inlines the defaults here,
  -- where the type is not known, and the derived instance only calls the
  -- result. Each value would then go through the generic representation,
  -- and the benchmark derive.contents.parseYaml.generic would be slower.
  parseYamlList = coerce (withSequence (parseItems (parseYaml @a)))
  -- The derived method applies this one to the dictionaries of the instance.
  -- The pragma inlines it there, so only the dictionary of 'FromYaml a'
  -- remains. Without the pragma, GHC 9.14 keeps the call with all the
  -- dictionaries, because its worker/wrapper does not drop unused
  -- dictionaries, and the inspection tests of the list decoders fail.
  {-# INLINE parseYamlList #-}

  -- A type that is its field passes the key to the field.
  parseYamlField k v
    | not (isTagged @f (yamlOptions @a))
    , Just p <- gFromUntaggedEntry (to . gWrap) (k, v) =
        coerce @(Parser a) p
    | otherwise = coerce (parseYaml @a v)

----------------------------------------
-- Options

-- | How a type is encoded and decoded.
--
-- The instances do not check the options. Options that give two keys of a
-- mapping or two constructors the same text encode values that do not read
-- back, as the fields below describe.
data YamlOptions = YamlOptions
  { fieldLabelModifier :: !(String -> String)
  -- ^ The key of a field from the name of the field. If two fields of a
  -- constructor get the same key, e.g. @fooBar@ and @foo_bar@ with
  -- 'snakeCase', the constructor encodes as a mapping with two equal keys,
  -- which does not read back.
  , constructorTagModifier :: !(String -> String)
  -- ^ The tag of a constructor from the name of the constructor. If two
  -- constructors get the same tag, e.g. @FooBar@ and @Foo_bar@ with
  -- 'snakeCase', the decoder reads the tag as the first of them.
  , tagKey :: !T.Text
  -- ^ The key of the tag, @tag@ by default. A record with a field of the same
  -- key encodes as a mapping with two equal keys, which does not read back.
  , contentsKey :: !T.Text
  -- ^ The key of the fields of a tagged constructor without field names,
  -- @contents@ by default. If it is the same as 'Yamlet.Generic.tagKey', such
  -- a constructor encodes as a mapping with two equal keys, which does not
  -- read back.
  , tagSingleConstructors :: !Bool
  -- ^ Give a type with one constructor a tag too, unless the constructor has
  -- no fields. Off by default.
  , omitNullFields :: !Bool
  -- ^ Leave out a field whose value is null, e.g. 'Nothing'. Off by default.
  -- A null field with comments stays, e.g. a t'Yamlet.Commented' field with
  -- the value 'Nothing' and a comment. So does a null field with an anchor,
  -- which an alias elsewhere can refer to.
  --
  -- With 'yamlDefault', a null field stays unless its default is null without
  -- comments. Otherwise the value would not read back: the decoder fills a
  -- missing key from the default, so e.g. a field 'Nothing' with the default
  -- @Just 1@ would read back as @Just 1@. A value that encodes as its default
  -- still reads back as the default, e.g. a field 'Nothing' of type
  -- @Maybe (Maybe a)@ with the default @Just Nothing@, because both encode as
  -- null.
  , rejectUnknownFields :: !Bool
  -- ^ Reject a key that is not a field of the constructor. On by default.
  --
  -- Turn it off for a document with keys that the type does not model, e.g.
  -- keys that only hold anchors. The decoder then ignores such keys, and an
  -- encode of the value leaves them out.
  }
  deriving stock (Generic)

-- | The options with the defaults that the fields of t'YamlOptions' name.
defaultYamlOptions :: YamlOptions
defaultYamlOptions =
  YamlOptions
    { fieldLabelModifier = id
    , constructorTagModifier = id
    , tagKey = "tag"
    , contentsKey = "contents"
    , tagSingleConstructors = False
    , omitNullFields = False
    , rejectUnknownFields = True
    }

-- | How a tagged constructor goes in a mapping. The choice is a type, see
-- 'SumEncoding', because it changes the shapes of the constructors that a
-- type can have.
data SumEncodingKind
  = -- | The fields go next to the tag, e.g. @{tag: Circle, radius: 1}@, and a
    -- field without a name goes under the contents key, e.g.
    -- @{tag: Forward, contents: 10}@.
    --
    -- An enumeration is a string, but a constructor without fields in a type
    -- with fields is a mapping, e.g. @{tag: Stop}@. To keep the encoding of an
    -- enumeration when you add a constructor with fields, use 'SingleField'.
    TaggedObject
  | -- | The entries of a field without a name go next to the tag, e.g.
    -- @{tag: Ahead, distance: 10}@ for @Ahead (Distance 10)@. The
    -- constructors must have one field without a name or no fields,
    -- otherwise the derived instances are a type error.
    --
    -- The field must encode as a mapping with a key and without an anchor,
    -- and no key can be the tag key or the contents key. Otherwise the
    -- constructor encodes as with 'TaggedObject'. Thus the field of a type
    -- with the same tag key stays under the contents key, and so does a
    -- mapping with an anchor that an alias can refer to.
    --
    -- Only the entries of the mapping go next to the tag. The tag and the
    -- comments of the mapping are lost, e.g. the tag of a
    -- v'Yamlet.Value.Tagged' value or the comments of a t'Yamlet.Commented'
    -- value. 'TaggedObject' keeps them.
    --
    -- The decoder reads a mapping with the contents key as with
    -- 'TaggedObject', and the other keys are unknown keys. Without the
    -- contents key, the other keys are the field, so a field that is not a
    -- mapping needs the contents key. An error at the mapping itself, e.g.
    -- that a mapping is not an integer, has a note at the tag that says so.
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
    -- The key of a constructor with named fields is not a field, so its
    -- comments are lost. The key of a field without a name goes to the
    -- field, e.g. for a 'Yamlet.Commented' value.
    SingleField
  deriving stock (Eq, Show)

-- | The configuration of the generic instances of t'Yamlet.Decode.FromYaml'
-- and t'Yamlet.Encode.ToYaml' for a type: the options and the default value.
class GenericYamlOptions a where
  -- | How a tagged constructor goes in a mapping, 'TaggedObject' by default.
  type SumEncoding a :: SumEncodingKind

  type SumEncoding a = TaggedObject

  -- | The options of the type, 'defaultYamlOptions' by default.
  yamlOptions :: YamlOptions
  yamlOptions = defaultYamlOptions

  -- | The value that gives the fields of missing keys, e.g. the default
  -- configuration. Without it, a missing key decodes like null. A key with
  -- the value null is not missing. For a sum type, the default applies only
  -- to the constructor of the default value.
  yamlDefault :: Maybe a
  yamlDefault = Nothing

-- | The value of a field without a default in 'yamlDefault'. A missing key
-- of the field is an error, also if the field accepts null, e.g. for a field
-- of type 'Maybe'. The encoder with 'Yamlet.Generic.omitNullFields' keeps
-- such a field.
--
-- The value throws an exception if it is evaluated, e.g. if you use
-- 'yamlDefault' directly. So the field must be 'requiredField' itself, not a
-- value that contains it, and the field must be lazy. The field of a newtype
-- is not lazy, because the newtype is the value of its field. With a strict
-- field or a newtype, the decoder and the encoder of the type throw each time
-- you use them. To require the key of a type with one field, declare it with
-- @data@.
--
-- With 'TaggedFlat', a field without a name has no key of its own, because
-- its keys are next to the tag. Thus the type of the field decides about
-- these keys. E.g. for a mapping with only the tag, the decoder reports the
-- missing keys of that type, or takes them from the 'yamlDefault' of that
-- type. To require a key of the field, use 'requiredField' in the default of
-- the type of the field.
requiredField :: a
requiredField = throw RequiredField

-- | The exception of 'requiredField'.
data RequiredField = RequiredField
  deriving stock (Show)

instance Exception RequiredField where
  displayException _ = "the field has no default"

-- | The field of a default, or 'Nothing' for 'requiredField'.
defaultField :: a -> Maybe a
-- An asynchronous exception in the handler of 'try' would be thrown again as
-- a synchronous one, and the shared result of the check would throw it to
-- every later decoder. With the mask, it arrives after the handler, where the
-- evaluation can resume.
defaultField x = case unsafeDupablePerformIO (uninterruptibleMask_ (try (evaluate x))) of
  Left RequiredField -> Nothing
  Right _ -> Just x

-- | An error if the 'yamlDefault' of the type has a 'requiredField' in a
-- strict field or in the field of a newtype. Without the check, each field
-- would look required, and a missing key would give the error of a field
-- that has a default.
checkDefault :: forall a. (GenericYamlOptions a, GDatatype (Rep a)) => ()
checkDefault = case yamlDefault @a of
  Just d
    | isNothing (defaultField d) ->
        error $ "requiredField in a strict field or a newtype of the default of " ++ gDatatypeName @(Rep a)
  _ -> ()
-- Without the pragma, the derived encoders and decoders of lists and fields
-- keep the generic dictionaries, and their inspection tests fail.
{-# INLINE checkDefault #-}

-- | The words of a name in lower case, separated by underscores, e.g.
-- @source_paths@ for @sourcePaths@ or @SourcePaths@, and @http_server@ for
-- @HTTPServer@. The rules are the same as for @camelTo2 \'_\'@ of aeson.
--
-- >>> map snakeCase ["sourcePaths", "SourcePaths", "HTTPServer", "ghcVersion2"]
-- ["source_paths","source_paths","http_server","ghc_version2"]
snakeCase :: String -> String
snakeCase = separateWords '_'

-- | Like 'snakeCase', but with hyphens, e.g. @source-paths@ for
-- @sourcePaths@.
--
-- >>> kebabCase "sourcePaths"
-- "source-paths"
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

-- | The layer of the data type at the top of a representation, and the
-- constructors below it. This class and the others of the representation
-- appear in the constraints of 'genericToYaml' and 'genericParseYaml'. Their
-- methods are internal.
--
-- An equality such as @Rep a ~ D1 d f@ would do the same, but for a type
-- without a 'Generic' instance, GHC would report that the equality fails
-- instead of the missing instance.
class GDatatype (r :: Type -> Type) where
  type Constructors r :: Type -> Type

  gUnwrap :: r p -> Constructors r p

  gWrap :: Constructors r p -> r p

  gDatatypeName :: String

instance KnownSymbol name => GDatatype (D1 (MetaData name m p nt) f) where
  type Constructors (D1 (MetaData name m p nt) f) = f

  gUnwrap = unM1

  gWrap = M1

  gDatatypeName = symbolVal (Proxy @name)

-- | The names and the number of the constructors of a representation.
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
          :$$: Text "Give them the same kind of fields, or use the sum encoding SingleField, where each constructor has its own value."
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

-- | The tag of the constructor with the name.
constructorTagOf :: forall name. KnownSymbol name => YamlOptions -> T.Text
constructorTagOf opts = constructorTag opts (symbolVal (Proxy @name))

-- | The default of the first constructor of a sum, if it is the default.
leftDefault :: Maybe ((f :+: g) p) -> Maybe (f p)
leftDefault def =
  def >>= \case
    L1 x -> Just x
    R1 _ -> Nothing

-- | The default of the second constructor of a sum, if it is the default.
rightDefault :: Maybe ((f :+: g) p) -> Maybe (g p)
rightDefault def =
  def >>= \case
    R1 x -> Just x
    L1 _ -> Nothing

-- | The first fields of the default of a product.
firstDefault :: Maybe ((f :*: g) p) -> Maybe (f p)
firstDefault = fmap (\(a :*: _) -> a)

-- | The second fields of the default of a product.
secondDefault :: Maybe ((f :*: g) p) -> Maybe (g p)
secondDefault = fmap (\(_ :*: b) -> b)

----------------------------------------
-- Fields

-- | The names and the number of the fields of a constructor.
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

----------------------------------------
-- Encoding

-- | The generic encoder, e.g. for an instance by hand that encodes some
-- values in another way.
--
-- >>> :{
-- data Size = Size {width :: Int, height :: Int}
--   deriving stock (Generic)
--   deriving anyclass (GenericYamlOptions)
-- instance ToYaml Size where
--   toYaml s
--     | s.width == 0 && s.height == 0 = toYaml ("empty" :: T.Text)
--     | otherwise = genericToYaml s
-- :}
--
-- >>> T.putStr (encodeText [Size 0 0, Size 1 2])
-- - empty
-- - width: 1
--   height: 2

-- GHC must inline the generic code in the derived method before the
-- specializer runs. Otherwise the specializer makes a copy of the code for
-- each node of the representation, and a large type takes several times
-- longer to compile. For the same reason, the top of the representation goes
-- to a plain function, not to a class with one method. GHC represents the
-- dictionary of such a class as a partial application of the method, and it
-- does not inline that.
genericToYaml
  :: forall a f
   . ( Generic a
     , GenericYamlOptions a
     , GDatatype (Rep a)
     , Constructors (Rep a) ~ f
     , GConstructors f
     , GEncoding (SumEncoding a) f
     , GToConstructor f
     )
  => a -> S.Node
genericToYaml x =
  -- Forcing the encoding forces the check of the shape, e.g. with deferred
  -- type errors in a test of the errors.
  let enc = gEncoding @(SumEncoding a) @f
      -- The keys of the options are texts that GHC does not know to be
      -- evaluated. Without the bang, it evaluates them in each branch of the
      -- constructors, and it keeps the generic representation of a sum, as
      -- the inspection test of encodeShape shows. GHC before 9.12 keeps it
      -- also with the bang.
      !opts = yamlOptions @a
  in enc `seq` checkDefault @a `seq` gToYaml opts enc (gUnwrap . from <$> yamlDefault @a) (gUnwrap (from x))
{-# INLINE genericToYaml #-}

-- The encoder takes the default for 'omitNullFields': it leaves out a null
-- field only if the default of the field is null without comments too.
-- Otherwise the decoder would fill the missing key from the default, and the
-- value would not read back.
gToYaml
  :: forall f p
   . ( GConstructors f
     , GToConstructor f
     )
  => YamlOptions -> SumEncodingKind -> Maybe (f p) -> f p -> S.Node
gToYaml opts enc def x
  | gNullary @f = scalar (String (gTag opts x))
  | otherwise = gToConstructor opts (if isTagged @f opts then Just enc else Nothing) def x
-- Without the pragma, GHC 9.2 does not inline this function, and GHC 9.4 does
-- not inline it for an enumeration. Then the inspection tests of these
-- derived encoders fail. Later versions inline it anyway.
{-# INLINE gToYaml #-}

-- | The encoder of the constructors of a representation.
class GToConstructor f where
  gTag :: YamlOptions -> f p -> T.Text

  -- | The constructor, with the tag in the given encoding.
  gToConstructor :: YamlOptions -> Maybe SumEncodingKind -> Maybe (f p) -> f p -> S.Node

  -- | The mapping entry under the key of a constructor without a tag, if it
  -- has one field without a name. The field gets the key, e.g. for the
  -- comments above the key of a 'Yamlet.Commented' field.
  gToUntaggedEntry :: S.Node -> f p -> Maybe (S.Node, S.Node)

instance GToConstructor V1 where
  gTag _ = \case {}

  gToConstructor _ _ _ = \case {}

  gToUntaggedEntry _ = \case {}

instance (GToConstructor f, GToConstructor g) => GToConstructor (f :+: g) where
  gTag opts = \case
    L1 x -> gTag opts x
    R1 x -> gTag opts x
  {-# INLINE gTag #-}

  gToConstructor opts flat def = \case
    L1 x -> gToConstructor opts flat (leftDefault def) x
    R1 x -> gToConstructor opts flat (rightDefault def) x
  {-# INLINE gToConstructor #-}

  -- A type with several constructors always has a tag.
  gToUntaggedEntry _ _ = Nothing

instance
  ( KnownSymbol name
  , GFields f
  , GToFields f
  )
  => GToConstructor (C1 (MetaCons name fixity isRecord) f)
  where
  gTag opts _ = constructorTagOf @name opts
  {-# INLINE gTag #-}

  gToConstructor opts tagging def c@(M1 x) = case tagging of
    Just SingleField
      | gNamed @f -> mapping [(string (gTag opts c), mapping (gToEntries opts (unM1 <$> def) x))]
      | otherwise -> case gToValue x of
          Nothing -> string (gTag opts c)
          Just _ -> mapping [gToEntry (string (gTag opts c)) x]
    Just enc
      | gNamed @f -> mapping (withTagEntry (gToEntries opts (unM1 <$> def) x))
      | otherwise -> case gToValue x of
          Nothing -> mapping (withTagEntry [])
          Just v
            | enc == TaggedFlat, Just entries <- flatEntries v -> mapping (withTagEntry entries)
            | otherwise -> mapping (withTagEntry [gToEntry (string opts.contentsKey) x])
    Nothing
      | gNamed @f -> mapping (gToEntries opts (unM1 <$> def) x)
      | otherwise -> fromMaybe (mapping []) (gToValue x)
    where
      withTagEntry :: [(S.Node, S.Node)] -> [(S.Node, S.Node)]
      withTagEntry entries = (opts.tagKey .= gTag opts c) : entries

      -- The entries of a field next to the tag, if the decoder can read them
      -- back. The field must be a mapping with a key, and no key can be the
      -- tag key or the contents key. The decoder reads a mapping with the
      -- contents key as the other form. An alias elsewhere can refer to the
      -- anchor of the mapping, so a mapping with an anchor keeps it under the
      -- contents key.
      flatEntries :: S.Node -> Maybe [(S.Node, S.Node)]
      flatEntries v = case v.content of
        S.MappingContent _ kvs
          | null kvs -> Nothing
          | isJust v.props.anchor -> Nothing
          | any (\(k, _) -> isKey opts.tagKey k || isKey opts.contentsKey k) kvs -> Nothing
          | otherwise -> Just kvs
        _ -> Nothing
  {-# INLINE gToConstructor #-}

  gToUntaggedEntry k (M1 x)
    | gNamed @f || gArity @f == 0 = Nothing
    | otherwise = Just (gToEntry k x)

-- | The encoder of the fields of a constructor.
--
-- The shape check allows named fields, no fields, or one field without a
-- name. The default methods are for the kind of fields that never calls
-- them.
class GToFields f where
  -- | The entries of the named fields, with the given default.
  gToEntries :: YamlOptions -> Maybe (f p) -> f p -> [(S.Node, S.Node)]
  gToEntries _ _ _ = []

  -- | The value of the only field without a name.
  gToValue :: f p -> Maybe S.Node
  gToValue _ = Nothing

  -- | The mapping entry of the only field without a name under the key, e.g.
  -- with the comments of a 'Yamlet.Commented' field on the contents key.
  gToEntry :: S.Node -> f p -> (S.Node, S.Node)
  gToEntry k x = (k, fromMaybe (mapping []) (gToValue x))

instance GToFields U1

instance (GToFields f, GToFields g) => GToFields (f :*: g) where
  gToEntries opts def (a :*: b) =
    gToEntries opts (firstDefault def) a ++ gToEntries opts (secondDefault def) b
  {-# INLINE gToEntries #-}

instance
  ( KnownSymbol name
  , ToYaml a
  )
  => GToFields (S1 (MetaSel (Just name) u s d) (Rec0 a))
  where
  gToEntries opts def (M1 (K1 x))
    | opts.omitNullFields && isNullNode (snd entry) && uncommented && unanchored && nullDefault = []
    | otherwise = [entry]
    where
      entry :: (S.Node, S.Node)
      entry = fieldKey @name opts .= x

      -- The comments would go away with the entry.
      uncommented :: Bool
      uncommented = (fst entry).comments == S.noComments && (snd entry).comments == S.noComments

      -- An alias elsewhere can refer to the anchor.
      unanchored :: Bool
      unanchored = isNothing (snd entry).props.anchor

      -- The decoder fills a missing key from the default.
      nullDefault :: Bool
      nullDefault = case def of
        Just (M1 (K1 d)) -> isNullDefault d
        Nothing -> True
  {-# INLINE gToEntries #-}

-- | The field of a default is null without comments, and not
-- 'requiredField'.
isNullDefault :: ToYaml a => a -> Bool
isNullDefault d = case toYaml <$> defaultField d of
  Just n -> isNullNode n && n.comments == S.noComments
  Nothing -> False
-- Not inlined, the call has only constant arguments, so GHC computes it once
-- for each field of a default. Inlined in the encoder, as in the @where@
-- clause of its caller, it ran on each encode, and the encode of records with
-- a default and 'omitNullFields' was slower and allocated more.
{-# NOINLINE isNullDefault #-}

instance ToYaml a => GToFields (S1 (MetaSel Nothing u s d) (Rec0 a)) where
  gToValue (M1 (K1 x)) = Just (toYaml x)

  gToEntry k (M1 (K1 x)) = toYamlField k x

----------------------------------------
-- Decoding

-- | The generic decoder, e.g. for an instance by hand with a check after the
-- decode.
--
-- >>> :{
-- data Range = Range {low :: Int, high :: Int}
--   deriving stock (Generic, Show)
--   deriving anyclass (GenericYamlOptions)
-- instance FromYaml Range where
--   parseYaml n = do
--     r <- genericParseYaml n
--     if r.low <= r.high then pure r else failAt n "expected low <= high"
-- :}
--
-- >>> decodeText @Range "low: 1\nhigh: 2\n"
-- Right (Range {low = 1, high = 2})
--
-- >>> either printErrors print (decodeText @Range "low: 3\nhigh: 2\n")
-- input.yaml:1:1: expected low <= high
--   |
-- 1 | low: 3
--   | ^

-- The code inlines in the derived method, for the reasons at 'genericToYaml'.
genericParseYaml
  :: forall a f
   . ( Generic a
     , GenericYamlOptions a
     , GDatatype (Rep a)
     , Constructors (Rep a) ~ f
     , GConstructors f
     , GEncoding (SumEncoding a) f
     , GFromConstructor f
     )
  => S.Node -> Parser a
genericParseYaml n =
  -- Forcing the encoding forces the check of the shape, e.g. with deferred
  -- type errors in a test of the errors.
  let enc = gEncoding @(SumEncoding a) @f
  in enc `seq` checkDefault @a `seq` gParseYaml (yamlOptions @a) enc (gUnwrap . from <$> yamlDefault @a) (to . gWrap) n
{-# INLINE genericParseYaml #-}

-- Each constructor applies 'to' to its own representation, e.g.
-- @to (M1 (L1 (M1 fields)))@, and the optimizer reduces this to the real
-- constructor in the same place. For this, the decoders of the constructors
-- take a continuation. It starts as 'to' after 'gWrap' and grows by 'M1',
-- 'L1' or 'R1' at each level of the sum.
--
-- In the direct style, each constructor returns its representation, the
-- branches meet in 'mplus', and 'to' comes after them. The optimizer then no
-- longer knows which branch produced the value, so the program builds 'L1',
-- 'R1' and ':*:' at run time and 'to' matches on them again.
--
-- The fields of one constructor need no continuation, because they build
-- their product in one place, and 'to' of the same branch consumes it.
--
-- The representation of the default goes down with the options, so that each
-- field finds its default value.
gParseYaml
  :: forall f p a
   . ( GConstructors f
     , GFromConstructor f
     )
  => YamlOptions -> SumEncodingKind -> Maybe (f p) -> (f p -> a) -> S.Node -> Parser a
gParseYaml opts enc def k n
  | gNullary @f =
      withName tags (\t -> fromMaybe (unknown n "value" t) (gFromTag opts k n t)) n
  | isTagged @f opts, enc == SingleField = single
  | isTagged @f opts = withMapping tagged n
  | otherwise = gFromUntagged opts def k n
  where
    tagged :: Object -> Parser a
    tagged o = case lookupKey opts.tagKey o of
      Nothing -> missingKey o opts.tagKey
      Just tn -> do
        t <- withName tags pure tn
        fromMaybe (unknown tn "tag" t) (gFromTagged opts (enc == TaggedFlat) def k t o)

    -- A constructor without fields is its tag, and another constructor is a
    -- mapping with its tag as the only key.
    single :: Parser a
    single = case view n of
      StringView t -> fromMaybe (withoutValue t) (gFromTag opts k n t)
      _ | S.MappingContent {} <- n.content -> withMapping singleEntry n
      _ -> typeMismatch "a string or a mapping with one key" n

    singleEntry :: Object -> Parser a
    singleEntry o = case objectEntries o of
      [(kn, v)] -> do
        t <- withName tags pure kn
        fromMaybe (unknown kn "constructor" t) (gFromSingle opts def k t (kn, v))
      _ : (kn, _) : _ -> failAt kn "expected a mapping with one key, but got a second key"
      [] -> failAt n "expected a mapping with one key, but got an empty mapping"

    -- A string that is the tag of a constructor with fields.
    withoutValue :: T.Text -> Parser a
    withoutValue t
      | t `elem` tags = failAt n $ "expected a mapping with the key " ++ showText t ++ ", because the constructor has fields"
      | otherwise = unknown n "constructor" t

    unknown :: S.Node -> String -> T.Text -> Parser a
    unknown node what = unknownName what tags node

    tags :: [T.Text]
    tags = map (constructorTag opts) (gConstructorNames @f)
{-# INLINE gParseYaml #-}

-- | The decoder of the constructors of a representation.
class GFromConstructor f where
  -- | The constructor without fields with the tag, from the node of the tag.
  gFromTag :: YamlOptions -> (f p -> a) -> S.Node -> T.Text -> Maybe (Parser a)

  -- | The constructor with the tag, from the mapping that holds the tag, with
  -- the flag of 'TaggedFlat'.
  gFromTagged :: YamlOptions -> Bool -> Maybe (f p) -> (f p -> a) -> T.Text -> Object -> Maybe (Parser a)

  -- | The constructor with the tag, from the only entry of a mapping, for
  -- 'SingleField'.
  gFromSingle :: YamlOptions -> Maybe (f p) -> (f p -> a) -> T.Text -> (S.Node, S.Node) -> Maybe (Parser a)

  -- | The only constructor, without a tag.
  gFromUntagged :: YamlOptions -> Maybe (f p) -> (f p -> a) -> S.Node -> Parser a

  -- | The only constructor, without a tag, from a mapping entry, if it has
  -- one field without a name. The field gets the key, e.g. for the comments
  -- above the key of a 'Yamlet.Commented' field.
  gFromUntaggedEntry :: (f p -> a) -> (S.Node, S.Node) -> Maybe (Parser a)

instance GFromConstructor V1 where
  gFromTag _ _ _ _ = Nothing

  gFromTagged _ _ _ _ _ _ = Nothing

  gFromSingle _ _ _ _ _ = Nothing

  gFromUntagged _ _ _ _ = fail "expected a type with constructors"

  gFromUntaggedEntry _ _ = Nothing

instance (GFromConstructor f, GFromConstructor g) => GFromConstructor (f :+: g) where
  gFromTag opts k n t = gFromTag opts (k . L1) n t `mplus` gFromTag opts (k . R1) n t
  {-# INLINE gFromTag #-}

  gFromTagged opts flat def k t o =
    gFromTagged opts flat (leftDefault def) (k . L1) t o
      `mplus` gFromTagged opts flat (rightDefault def) (k . R1) t o
  {-# INLINE gFromTagged #-}

  gFromSingle opts def k t entry =
    gFromSingle opts (leftDefault def) (k . L1) t entry
      `mplus` gFromSingle opts (rightDefault def) (k . R1) t entry
  {-# INLINE gFromSingle #-}

  -- A type with several constructors always has a tag.
  gFromUntagged _ _ _ _ = fail "expected a tag"

  gFromUntaggedEntry _ _ = Nothing

instance
  ( KnownSymbol name
  , GFields f
  , GFromFields f
  )
  => GFromConstructor (C1 (MetaCons name fixity isRecord) f)
  where
  gFromTag opts k n t
    | t == tag && gArity @f == 0 = Just (k . M1 <$> gFromValue n)
    | otherwise = Nothing
    where
      tag :: T.Text
      tag = constructorTagOf @name opts
  {-# INLINE gFromTag #-}

  gFromTagged opts flat def k t o
    | t == constructorTagOf @name opts = Just (k . M1 <$> fromObject opts flat [opts.tagKey] (unM1 <$> def) o)
    | otherwise = Nothing
  {-# INLINE gFromTagged #-}

  gFromSingle opts def k t entry@(kn, v)
    | t /= constructorTagOf @name opts = Nothing
    | gNamed @f = Just (withMapping (fmap (k . M1) . fromObject opts False [] (unM1 <$> def)) v)
    | gArity @f == 0 = Just (failAt kn $ "expected the string " ++ showText t ++ ", because the constructor has no fields")
    | otherwise = Just (k . M1 <$> gFromEntry entry)
  {-# INLINE gFromSingle #-}

  gFromUntagged opts def k n
    | gNamed @f || gArity @f == 0 = withMapping (fmap (k . M1) . fromObject opts False [] (unM1 <$> def)) n
    | otherwise = k . M1 <$> gFromValue n
  {-# INLINE gFromUntagged #-}

  gFromUntaggedEntry k entry
    | gNamed @f || gArity @f == 0 = Nothing
    | otherwise = Just (k . M1 <$> gFromEntry entry)

-- | The fields of a constructor from a mapping. The given keys, e.g. the tag
-- key, are no fields but valid keys.
fromObject
  :: forall f p
   . ( GFields f
     , GFromFields f
     )
  => YamlOptions -> Bool -> [T.Text] -> Maybe (f p) -> Object -> Parser (f p)
fromObject opts flat keys def o
  | gNamed @f || gArity @f == 0 = checked (gNames @f opts) (gFromObject opts def o)
  | flat
  , not (null others)
  , not (any (isKey opts.contentsKey . fst) others) =
      flatField
  | otherwise = checked [opts.contentsKey] $ case M.lookup opts.contentsKey o.index of
      Just entry -> gFromEntry entry
      -- A missing contents key is null, if the fields accept null. A flat
      -- field can also have only optional keys. A flat field has no key to
      -- require.
      Nothing
        | Just fields <- gDefaultValue =<< def -> pure fields
        | isJust def, not flat -> missingKey o opts.contentsKey
        | flat -> maybe flatField pure (succeeds gFromValue nullNode)
        | otherwise -> maybe (missingKey o opts.contentsKey) pure (succeeds gFromValue nullNode)
  where
    -- The fields, with the errors of the unknown keys if the options reject
    -- them.
    checked :: [T.Text] -> Parser (f p) -> Parser (f p)
    checked fields = (when opts.rejectUnknownFields (rejectUnknownKeys (keys ++ fields) o) *>)

    -- An error at the mapping itself, e.g. of a field that is not a mapping,
    -- does not show that the field is the mapping, so a note at the tag says
    -- it.
    flatField :: Parser (f p)
    flatField =
      withNote
        (objectNode o).offset
        ( maybe S.noOffset (.offset) (lookupKey opts.tagKey o)
        , "without the key " ++ showText opts.contentsKey ++ ", the other keys of this mapping are the field"
        )
        merged

    -- The field decodes from the mapping without the given keys, and without
    -- the comments of the mapping, which the record drops.
    merged :: Parser (f p)
    merged =
      let n = objectNode o
          style = case n.content of
            S.MappingContent s _ -> s
            _ -> S.Block
      in gFromValue (S.Node n.offset n.endOffset n.props S.noComments (S.MappingContent style others))

    -- The duplicates of a key go too. The mapping has their errors, and a
    -- field of a recursive type would give them again at each level.
    others :: [(S.Node, S.Node)]
    others
      | o.duplicates = filter (\(k, _) -> not (any (`isKey` k) keys)) (objectEntries o)
      | otherwise = foldr removeKey (objectEntries o) keys

    -- The keys are unique, so the entries after the match stay shared.
    removeKey :: T.Text -> [(S.Node, S.Node)] -> [(S.Node, S.Node)]
    removeKey key = \case
      kv@(k, _) : kvs
        | isKey key k -> kvs
        | otherwise -> kv : removeKey key kvs
      [] -> []
{-# INLINE fromObject #-}

-- | The decoder of the fields of a constructor.
--
-- The shape check allows named fields, no fields, or one field without a
-- name. The default methods are for the kind of fields that never calls
-- them.
class GFromFields f where
  -- | The fields from a mapping, with the given default for missing keys.
  gFromObject :: YamlOptions -> Maybe (f p) -> Object -> Parser (f p)
  gFromObject _ _ o = fail $ "expected a field without a name in " ++ describeNode (objectNode o)

  -- | The only field without a name from its value.
  gFromValue :: S.Node -> Parser (f p)
  gFromValue n = fail $ "expected named fields in " ++ describeNode n

  -- | The only field from a mapping entry, with the key, e.g. for the
  -- comments of a 'Yamlet.Commented' field under the contents key.
  gFromEntry :: (S.Node, S.Node) -> Parser (f p)
  gFromEntry (_, v) = gFromValue v

  -- | The only field without a name from a default, or 'Nothing' for
  -- 'requiredField'.
  gDefaultValue :: f p -> Maybe (f p)
  gDefaultValue = Just

-- The value of a constructor without fields is its tag.
instance GFromFields U1 where
  gFromObject _ _ _ = pure U1

  gFromValue _ = pure U1

instance (GFromFields f, GFromFields g) => GFromFields (f :*: g) where
  gFromObject opts def o =
    (:*:)
      <$> gFromObject opts (firstDefault def) o
      <*> gFromObject opts (secondDefault def) o
  {-# INLINE gFromObject #-}

instance
  ( KnownSymbol name
  , FromYaml a
  )
  => GFromFields (S1 (MetaSel (Just name) u s d) (Rec0 a))
  where
  gFromObject opts def o =
    M1 . K1 <$> case M.lookup key o.index of
      Just entry -> parseEntry entry
      Nothing -> case def of
        Just (M1 (K1 d))
          | Just x <- defaultField d -> x <$ findKey o key
          | otherwise -> missingKey o key
        -- A missing field is null, if its type accepts null.
        Nothing -> maybe (missingKey o key) (<$ findKey o key) (succeeds parseYaml nullNode)
    where
      key :: T.Text
      key = fieldKey @name opts
  {-# INLINE gFromObject #-}

instance FromYaml a => GFromFields (S1 (MetaSel Nothing u s d) (Rec0 a)) where
  gFromValue n = M1 . K1 <$> parseNode parseYaml n

  gFromEntry entry = M1 . K1 <$> parseEntry entry

  gDefaultValue (M1 (K1 x)) = M1 . K1 <$> defaultField x

-- | The key is a string with the text.
isKey :: T.Text -> S.Node -> Bool
isKey key k = case stringValue k of
  Just t -> t == key
  _ -> False

-- $setup
-- >>> import Data.Text qualified as T
-- >>> import Data.Text.IO qualified as T
-- >>> import Yamlet
-- >>> printErrors = mapM_ (putStrLn . prettyError "input.yaml")
