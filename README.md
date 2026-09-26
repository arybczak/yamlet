# yamlet

A YAML 1.2.2 library written in Haskell.

## Features

- The parser follows the grammar of the YAML 1.2.2 specification. It passes
  all 402 cases of the [YAML test suite](https://github.com/yaml/yaml-test-suite)
  (release `data-2022-01-17`), for both the parse events and the JSON values.
- Errors give the line and the column of the problem, both counted from 1, and
  an excerpt of the input. Messages name the kinds of values in plain words,
  e.g. `expected a list, but got an integer`.
- Mappings keep the order of their keys, on input and on output.
- Instances of `FromYaml` and `ToYaml` for the common types. They use the
  same formats as the instances of aeson, with one difference: the keys of a
  map keep their type, e.g. `1: a`, while JSON writes every key as a string.
- The syntax tree keeps the comments and the empty lines, so a program can
  read a file, change it and write it back with its comments.
- Floating-point numbers are exact, e.g. `0.1` is exactly one tenth. They
  convert to `Scientific` without loss and to `Double` on request.
- If a string reads back the same as a plain scalar, the encoder does not
  quote it. Thus `dist-newstyle` stays unquoted.
- The input can be UTF-8, UTF-16 or UTF-32. The library detects the encoding
  as the specification describes.

## Modules

- `Yamlet`: decoding with the `FromYaml` class and encoding with the `ToYaml`
  class.
- `Yamlet.Node`: the representation graph, with resolved tags and aliases.
- `Yamlet.Syntax`: the syntax tree, with styles, anchors and unresolved tags.
  It keeps the comments and the empty lines, each at a node that the rules in
  its documentation choose. The module has a parser and a renderer for it.
- `Yamlet.Schema`: the rules of the core schema, e.g. to check how a plain
  scalar reads back.

## Untrusted input

The decoder is safe to use on untrusted input. The time to decode a
document is close to linear in its size, and the memory is linear in its
size.

A decoded value is never much larger than its text. So the library limits
the two parts of the syntax that let a short text stand for a large value,
aliases and exponents:

- The aliases of a document can add at most 100000 nodes. For a document
  with more than 100000 nodes, they can add as many nodes as the document
  has. A document beyond the limit is an error. So a small document with
  aliases to aliases cannot expand to billions of nodes.
- The exponent of a float can make its value at most 1000 digits larger
  than its text. So `1e999999999` is an error, and a program cannot convert
  it to an integer with a billion digits. The instances for `Fixed` and the
  durations apply the same limit with `withBoundedScientific`. Use it in your
  own instances for exact types too.

The library also applies these rules:

- Integers and floats can have any number of digits. The time to read and
  write them is close to linear in the number of digits.
- The check for duplicate keys takes close to linear time, also for keys
  that are large collections or aliases.
- Deeply nested collections, e.g. 100000 levels of flow sequences, take
  linear time to parse.
- A fraction is reduced as an `Integer`, and its parts must fit in the
  target type.
- The decoded values do not keep the input in memory, because the decoder
  copies their texts.

The program must still limit the size of the input, because the memory
grows with it.

## Performance

The benchmark in `bench/` uses three generated inputs:

- `config`: a list of records in block style, as in a configuration file.
- `json`: a list of records in JSON syntax.
- `text`: a mapping of long multi-line strings.

For each input, every library decodes the YAML into the same Haskell type and
encodes a value of that type back to YAML. The times below come from GHC
9.10.3 on a Ryzen 9950X3D. Each benchmark ran in its own process, pinned to
one core of the CCD with the 3D V-cache. The `yaml` package uses the libyaml C
library and converts the data by way of an aeson `Value`.

Decoding:

| Input              | yamlet | HsYAML  | yaml   |
|--------------------|--------|---------|--------|
| `config`, 1105 KiB | 37 ms  | 3021 ms | 115 ms |
| `json`, 432 KiB    | 20 ms  | 2444 ms | 60 ms  |
| `text`, 834 KiB    | 7.2 ms | 640 ms  | 13 ms  |

Encoding:

| Input              | yamlet | HsYAML | yaml   |
|--------------------|--------|--------|--------|
| `config`, 1105 KiB | 18 ms  | 39 ms  | 56 ms  |
| `json`, 432 KiB    | 11 ms  | 19 ms  | 34 ms  |
| `text`, 834 KiB    | 2.9 ms | 10 ms  | 9.6 ms |

## Tests

The test suite reads the data of the YAML test suite from
`tests/fixtures/yaml-test-suite`. The repository contains the data. To
download it again, e.g. after you change the release in the script, run
this command:

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
