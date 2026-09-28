# yamlet

A YAML 1.2.2 library written in Haskell.

## Features

- The parser follows the grammar of the YAML 1.2.2 specification. It passes
  all 402 cases of the [YAML test suite](https://github.com/yaml/yaml-test-suite)
  (release `data-2022-01-17`), for both the parse events and the JSON values.
- Errors give the line and the column of the problem, both counted from 1, and
  an excerpt of the input. A decoder error also gives the keys and indices
  that lead to the problem, e.g. `jobs[1].name`. Messages name the kinds of
  values in plain words, e.g. `expected a list, but got an integer`.
- The decoder reports the errors of independent parts together, e.g. every
  bad field of a record, every bad item of a list and every unknown key. A
  syntax error stops the parser at the first one.
- Mappings keep the order of their keys, on input and on output.
- The encoder writes output that common YAML 1.1 parsers read the same way:
  PyYAML, Ruby's Psych and go-yaml v2, which Kubernetes uses. It quotes the
  strings that these parsers read as other types, e.g. `yes`, `22:22`,
  `1,000` and `2024-01-01`. The decoder follows only the YAML 1.2 rules.
- Instances of `FromYaml` and `ToYaml` for the common types. They use the
  same formats as the instances of aeson, with one difference: the keys of a
  map keep their type, e.g. `1: a`, while JSON writes every key as a string.
  The decoders are stricter than aeson. They reject some values that aeson
  converts, e.g. `1.0` for an `Int`, `null` for a `Double`, `0.5` for a
  `Rational` and a duplicate item of a `Set`.
- Generic instances with the formats of aeson, for fewer shapes of types. A
  constructor cannot have several fields without names. Such a type is a
  compile error. A sum type is a mapping with a tag, e.g.
  `{tag: Circle, radius: 1}`, or a mapping with the constructor as its only
  key, e.g. `{Circle: {radius: 1}}`.
- The syntax tree keeps the comments and the empty lines, so a program can
  read a file, change it and write it back with its comments.
- A decoded type can keep a part of a document as a `Node`. The encoder
  writes it back as it was written, with its comments and styles. A field of
  type `Commented a` also keeps the comments of its entry, e.g. the comment
  above `permissions:`. A field of type `Located a` keeps the position of its
  value. Thus a check after the decode can give an error with the line, the
  column and the path.
- Floating-point numbers are exact, e.g. `0.1` is exactly one tenth. They
  convert to `Scientific` without loss and to `Double` on request.
- The input can be UTF-8, UTF-16 or UTF-32. The library detects the encoding
  as the specification describes.

## Modules

- `Yamlet`: decoding with the `FromYaml` class and encoding with the `ToYaml`
  class. The instances read and write the nodes of the syntax tree.
- `Yamlet.Value`: the values of documents, with resolved tags and aliases,
  e.g. for a document whose structure a program does not know.
- `Yamlet.Syntax`: the syntax tree, with styles, anchors and unresolved tags.
  It keeps the comments and the empty lines, each at a node that the rules in
  its documentation choose. The module has a parser and a renderer for it.
- `Yamlet.Schema`: the rules of the core schema, e.g. to check how a plain
  scalar reads back.

## Untrusted input

The decoder is safe to use on untrusted input. The time to decode a
document is close to linear in its size, and the memory is linear in its
size.

A decoded value is never much larger than its text. Two parts of the syntax
can let a short text stand for a large value: aliases and exponents. The
library limits both:

- A small document with aliases to aliases can expand to billions of nodes.
  To prevent this, the aliases of a document can add at most 100000 nodes.
  For a document with more than 100000 nodes, they can add as many nodes as
  the document has. A document beyond the limit is an error.
- A program that converts `1e999999999` to an integer gets a billion digits.
  To prevent this, the exponent of a float can make its value at most 1000
  digits larger than its text. A float beyond the limit is an error. The
  instances for `Fixed` and the durations apply the same limit with
  `withBoundedScientific`. Use it in your own instances for exact types too.

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
  copies their texts. This holds for a kept `Node` too.
- With the instances of the library and the derived instances, the number
  of decoder errors grows at most linearly with the size of the document.
  The time to locate the errors in the input and to find their paths is
  close to linear, also for many errors on one line.

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
| `config`, 1105 KiB | 29 ms  | 3021 ms | 115 ms |
| `json`, 432 KiB    | 17 ms  | 2444 ms | 60 ms  |
| `text`, 834 KiB    | 6.9 ms | 640 ms  | 13 ms  |

Encoding:

| Input              | yamlet | HsYAML | yaml   |
|--------------------|--------|--------|--------|
| `config`, 1105 KiB | 20 ms  | 39 ms  | 56 ms  |
| `json`, 432 KiB    | 11 ms  | 19 ms  | 34 ms  |
| `text`, 834 KiB    | 3.0 ms | 10 ms  | 9.6 ms |

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
