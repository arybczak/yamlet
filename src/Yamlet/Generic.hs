-- | Instances of t'Yamlet.Decode.FromYaml' and t'Yamlet.Encode.ToYaml' from
-- the t'GHC.Generics.Generic' representation of a type:
--
-- @
-- data Server = Server {host :: Text, port :: Int}
--   deriving stock (Generic)
--   deriving anyclass (GenericYaml, FromYaml, ToYaml)
-- @
--
-- A type with other options defines 'yamlOptions' in its instance of
-- 'GenericYaml'.
--
-- = Encoding
--
-- * A record is a mapping of its fields, e.g. @{host: localhost, port: 80}@.
--
-- * A type whose constructors have no fields is a string with the name of
--   the constructor, e.g. @TurnLeft@.
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
--   @Ahead (Distance 10)@:
--
-- @
-- instance GenericYaml Step where
--   type SumEncoding Step = TaggedFlat
-- @
--
-- = Shapes
--
-- Every constructor has no fields, one field without a name, or named
-- fields. A type with several constructors cannot mix named fields with a
-- field without a name, but a constructor without fields fits with both.
-- 'TaggedFlat' needs constructors with a field without a name. Another
-- type is a compile error that names the constructors, e.g. for a
-- constructor with several fields without names. Give such fields names, or
-- put them in a tuple.
--
-- = Missing keys
--
-- A missing field takes its value from the 'yamlDefault' of the type that
-- has the field, if that type has a default. Otherwise it decodes like a
-- field with the value null, and so does a missing contents key. Thus a
-- field of type 'Maybe' is optional, and a missing field of another type is
-- an error, also if the type of the field has a default.
--
-- A type with a default configuration derives the decoder like this:
--
-- @
-- instance GenericYaml Config where
--   yamlDefault = Just defaultConfig
-- @
--
-- A present key that holds a mapping takes the missing keys of that mapping
-- from the default of its own type, not from the outer default. The same
-- holds with 'TaggedFlat': the keys of a field without a name are next to
-- the tag, but they belong to the field. The outer default
-- applies to such a field only if the constructor has no keys besides the
-- tag.
--
-- An explicit null is no missing key, so it goes to the decoder of the
-- field, e.g. @proxy: null@ gives 'Nothing' for a field of type 'Maybe'. So
-- does @proxy:@ without a value. An empty document is null too, so a type
-- with a default does not decode from it.
module Yamlet.Generic
  ( YamlOptions (..)
  , defaultYamlOptions
  , GenericYaml (..)
  , SumEncodingKind (..)

    -- * Modifiers
  , snakeCase
  , kebabCase
  ) where

import Yamlet.Internal.Generic
