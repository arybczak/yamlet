-- | Instances of 'Yamlet.Decode.FromYaml' and 'Yamlet.Encode.ToYaml' from the
-- 'GHC.Generics.Generic' representation of a type:
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
--   e.g. @{tag: Circle, radius: 1}@. A constructor without field names has
--   its field under the contents key, e.g. @{tag: Forward, contents: 10}@, or
--   a list of its fields if it has several.
--
-- * A type with one constructor without field names is its field, or a list
--   of its fields if it has several.
--
-- * With 'FlattenFields', the entries of a field without a name go in the
--   mapping of the constructor, e.g. @{tag: Ahead, distance: 10}@ for
--   @Ahead (Distance 10)@:
--
-- @
-- instance GenericYaml Step where
--   type FlattenFields Step = True
-- @
--
-- = Missing keys
--
-- A missing field takes its value from 'yamlDefault', if the type has a
-- default. Otherwise it decodes like a field with the value null, and so
-- does a missing contents key. Thus a field of type 'Maybe' is optional, and
-- a missing field of another type is an error.
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
-- holds with 'FlattenFields': the keys of a field without a name are next to
-- the tag, but they belong to the field. The outer default
-- applies to such a field only if the constructor has no keys besides the
-- tag.
--
-- An explicit null is no missing key, so it goes to the decoder of the
-- field, e.g. @proxy: null@ gives 'Nothing' for a field of type 'Maybe'.
module Yamlet.Generic
  ( YamlOptions (..)
  , defaultYamlOptions
  , GenericYaml (..)

    -- * Modifiers
  , snakeCase
  , kebabCase
  ) where

import Yamlet.Internal.Generic
