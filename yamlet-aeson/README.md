# yamlet-aeson

Decoding and encoding of YAML with
[yamlet](https://hackage.haskell.org/package/yamlet) and the instances of
[aeson](https://hackage.haskell.org/package/aeson), for the types that have
no instances of yamlet, e.g. the types of other libraries.

- A value in `ViaAeson` decodes and encodes with its instances of aeson, so
  the functions of yamlet work with it, e.g.
  `decodeFile @(ViaAeson Config)`.
- A type with instances of aeson derives its instances of yamlet via
  `ViaAeson`, e.g. to be a field of a type with instances of yamlet.
- An error of a decoder of aeson points to the node that caused it, with the
  line, the column and the path. An error of a key of a map points to the
  value of the key, and its path names the key.
- The encoder keeps the order of the fields of a type whose instance defines
  `toEncoding`, e.g. with `genericToEncoding`.
- The package also has the instances of yamlet for the `Value` of aeson.

## Example

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

import Data.Aeson qualified as A
import Data.Text (Text)
import Yamlet
import Yamlet.Aeson

data Server = Server {port :: Int, host :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (A.FromJSON)

instance A.ToJSON Server where
  toEncoding = A.genericToEncoding A.defaultOptions

main :: IO ()
main = do
  result <- decodeFile @(ViaAeson Server) path
  case result of
    Left errs -> mapM_ (putStrLn . prettyError path) errs
    Right (ViaAeson server) -> print server
  where
    path :: FilePath
    path = "server.yaml"
```

With `port: http` in the file, the program prints:

```
server.yaml:1:7: port: parsing Int failed, expected Number, but encountered String
  |
1 | port: http
  |       ^
```

## Conversion

A YAML document converts to an aeson `Value` as follows:

- A key is the text of its scalar, e.g. `"0x10"` for `0x10` and `"~"` for
  `~`, as in the yaml package. Two keys with the same text are an error,
  e.g. `1` and `"1"`. A key that is a collection is an error.
- A key `<<` is an ordinary key, because the merge keys of YAML 1.1 are
  not supported.
- `.inf` and `-.inf` are the strings `"+inf"` and `"-inf"`, and `.nan` is
  null, which the instances of aeson for `Double` and `Float` read and
  write.
- A tag that is not of the core schema makes a scalar a string, e.g.
  `!secret 123` is the string `"123"`. The yaml package reads it as the
  number 123. On a collection, such a tag does not matter.

An instance of aeson can convert two different keys to the same key and then
keep only one of the pairs, as it does for JSON, e.g. `1` and `1.0` for a
`Map Int`. The instances of yamlet for maps reject such keys.

A `Value` converts to YAML as aeson writes it in JSON, e.g. the keys of a
`Map Int` are strings, which the encoder quotes because they look like
numbers.

## Order of keys

The encoder writes the keys of a mapping in the order of `toEncoding`. The
default `toEncoding` goes through `toJSON`, so the keys come in the order of
an aeson object, which is sorted by default.

The decoder of aeson gets the keys in no order, because an aeson object has
none. To keep the order of a mapping, a program decodes the mapping with a
decoder of yamlet, e.g. with `withMapping` and `objectEntries`. Its values
can still decode via `ViaAeson`.
