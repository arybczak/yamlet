# yamlet

[![CI](https://github.com/arybczak/yamlet/actions/workflows/haskell-gha.yml/badge.svg?branch=master)](https://github.com/arybczak/yamlet/actions/workflows/haskell-gha.yml?query=branch%3Amaster)

A YAML 1.2.2 library written in Haskell. Main features:

- Conformance: the parser passes all cases of the
  [YAML test suite](https://github.com/yaml/yaml-test-suite).
- Performance: decoding and encoding are very fast, see the
  [benchmarks](#performance).
- Decoding and encoding with the classes `FromYaml` and `ToYaml`, with
  instances for common types.
- Instances for your own data types, derived via `GenericYaml`.
  Inspection tests check that the generic representation optimizes away
  for common shapes of data types.
- Errors with the line, the column and the path of the problem, e.g.
  `jobs[1].name`. The decoder reports the errors of independent parts
  together, e.g. every bad field of a record.
- A syntax tree that keeps the comments and the empty lines. A program can
  change a file and write it back with its comments, and a decoded value
  can keep a part of the document as it was written.
- Output that YAML 1.1 parsers read the same way, e.g. PyYAML and go-yaml
  v2, which Kubernetes uses.
- Safe for untrusted input. The time of a decode is close to linear in the
  size of the input, and the memory is linear.

The library supports GHC 9.2 and later.

## Example

A configuration type derives its decoder. The options reject unknown keys,
and the default gives the paths when the key is missing:

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}

import Data.Text (Text)
import Yamlet

data Config = Config
  { name :: Text
  , paths :: [FilePath]
  }
  deriving stock (Generic, Show)
  deriving (FromYaml) via GenericYaml Config

instance GenericYamlOptions Config where
  yamlOptions = defaultYamlOptions {rejectUnknownFields = True}
  yamlDefault = Just Config {name = requiredField, paths = ["."]}

main :: IO ()
main = do
  result <- decodeFile @Config "config.yaml"
  case result of
    Left errs -> mapM_ (putStrLn . prettyError "config.yaml") errs
    Right config -> print config
```

For this file:

```yaml
paths:
- src
- 42
port: 80
```

the program prints every error:

```
config.yaml:1:1: missing key "name"
  |
1 | paths:
  | ^
config.yaml:3:3: paths[1]: expected a string, but got an integer, quote the value, e.g. '42'
  |
3 | - 42
  |   ^
config.yaml:4:1: unknown key "port", expected one of: name, paths
  |
4 | port: 80
  | ^
```

A decoded type can also keep comments and parts of a document as they were
written:

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}

import Data.Text (Text)
import Yamlet

data Workflow = Workflow
  { name :: Commented Text
  , matrix :: Node
  }
  deriving stock (Generic)
  deriving anyclass (GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml Workflow
```

This input decodes to a `Workflow`, and `encodeText` writes it back
unchanged:

```yaml
# The name in the UI.
name: build # short
matrix:
  # Each system runs the jobs.
  os: [linux, macos]
  ghc: ['9.10', '9.12']
```

## Coming from the yaml package

The [yaml](https://hackage.haskell.org/package/yaml) package decodes and
encodes with the instances of aeson. The instances of yamlet, the generic
ones too, read and write the same YAML, so files written for it keep
working, with these exceptions:

- The yaml package reads `y`, `yes`, `on`, `n`, `no` and `off` as booleans,
  as YAML 1.1 does. Here they are strings, and a decoder that expects a
  boolean suggests `true` or `false`.
- The yaml package merges the entries of a `<<` key into its mapping, as
  YAML 1.1 does. YAML 1.2 has no merge keys, so here `<<` is an ordinary key.
- The keys of a map keep their type. aeson writes every key as a string, so
  the yaml package writes a key of a `Map Int` as `'1'`, which does not
  decode here. An `IntMap` and a map with keys that aeson cannot write as
  strings, e.g. a `Map (Int, Int)`, are mappings here and lists of pairs
  there.
- A value must have the YAML type of its Haskell type. The decoders reject
  some values that aeson converts, e.g. `1.0` for an `Int`, `0.5` for a
  `Rational` and `null` for a `Double`.
- A generic type with one constructor without fields is the name of the
  constructor, e.g. `Unit`, where aeson writes `[]`. With the encoding
  `SingleField`, a constructor without fields is its name, e.g. `Dot`, where
  aeson writes `{Dot: []}`.

The documentation of the instances and of the generic options describes the
remaining details.

## Known limits

- No streaming. The parser reads the whole input, and a decode of a stream
  parses all its documents before it decodes the first one. The library has
  no interface to the events of the parser.
- Only YAML 1.2. A document with `%YAML 1.1` follows the rules of YAML 1.2,
  e.g. `yes` is a string and `0755` is the integer 755. The merge keys of
  YAML 1.1 (`<<`) are not supported.
- The renderer writes its own layout. A file written back keeps its
  comments, empty lines, styles and anchors, but not its indentation or the
  spaces between tokens.
- Limits for untrusted input. The aliases of a document can add at most
  100000 nodes and characters, or as many as the document has if it has
  more. A float with an exponent beyond the range from -1000 to 1000 is an
  error, e.g. `1e1001`. The library does not limit the size of the input,
  so a program that reads untrusted input must limit it.

## Performance

Each library decodes three generated inputs into the same Haskell type and
encodes a value of that type back to YAML:

- `config`: a list of records in block style, as in a configuration file.
- `json`: a list of records in JSON syntax.
- `text`: a mapping of long multi-line strings.

Decoding:

| Input              | yamlet | HsYAML  | yaml   |
|--------------------|--------|---------|--------|
| `config`, 1105 KiB | 27 ms  | 3017 ms | 116 ms |
| `json`, 432 KiB    | 15 ms  | 2420 ms | 61 ms  |
| `text`, 834 KiB    | 7.1 ms | 637 ms  | 13 ms  |

Encoding:

| Input              | yamlet | HsYAML | yaml   |
|--------------------|--------|--------|--------|
| `config`, 1105 KiB | 21 ms  | 39 ms  | 57 ms  |
| `json`, 432 KiB    | 12 ms  | 19 ms  | 34 ms  |
| `text`, 834 KiB    | 3.2 ms | 11 ms  | 9.5 ms |

The times come from GHC 9.10.3 on a Ryzen 9950X3D. Each benchmark ran in its
own process, pinned to one core of the CCD with the 3D V-cache. The `yaml`
package uses the libyaml C library and converts the data by way of an aeson
`Value`. The section [Development](#development) shows how to run the
benchmarks.

## Development

The test suite reads the data of the YAML test suite, release
`data-2022-01-17`, from `tests/fixtures/yaml-test-suite`. The repository
contains the data. To download it again, e.g. after you change the release
in the script, run this command:

```
scripts/fetch-test-suite.sh
```

The file `tests/fixtures/error-messages.txt` holds the expected error
message for each invalid input of the YAML test suite. If you change an
error message on purpose, update the file with this command and review the
diff:

```
YAMLET_ACCEPT_ERRORS=1 cabal test
```

To run the benchmarks of the section [Performance](#performance) and print
its tables, run this command:

```
scripts/bench-readme.sh
```

The script runs each benchmark in its own process and pins it to core 2. To
use another core, set the `CORE` variable, e.g.
`CORE=4 scripts/bench-readme.sh`.
