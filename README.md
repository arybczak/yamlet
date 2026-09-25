# yamlet

A YAML 1.2.2 library written in Haskell, with few dependencies.

## Features

- The parser follows the grammar of the YAML 1.2.2 specification. It passes
  all 402 cases of the [YAML test suite](https://github.com/yaml/yaml-test-suite)
  (release `data-2022-01-17`), for both the parse events and the JSON values.
- Errors give the line and the column of the problem, both counted from 1, and
  an excerpt of the input. Messages name the kinds of values in plain words,
  e.g. `expected a list, but got an integer`.
- Mappings keep the order of their keys, on input and on output.
- The syntax tree keeps the comments and the empty lines, so a program can
  read a file, change it and write it back with its comments.
- Floating-point numbers are exact, e.g. `0.1` is exactly one tenth. They
  convert to `Scientific` without loss and to `Double` on request.
- If a string reads back the same as a plain scalar, the encoder does not
  quote it. Thus `dist-newstyle` stays unquoted.
- The input can be UTF-8, UTF-16 or UTF-32. The library detects the encoding
  as the specification describes.

## Modules

- `Yamlet`: decoding with the `FromYAML` class and encoding with the `ToYAML`
  class.
- `Yamlet.Node`: the representation graph, with resolved tags and aliases.
- `Yamlet.Syntax`: the syntax tree, with styles, anchors and unresolved tags.
  It keeps the comments and the empty lines, each at a node that the rules in
  its documentation choose. The module has a parser and a renderer for it.
- `Yamlet.Schema`: the rules of the core schema, e.g. to check how a plain
  scalar reads back.

## Performance

The benchmark in `bench/` uses three generated inputs:

- `config`: a list of records in block style, as in a configuration file.
- `json`: a list of records in JSON syntax.
- `text`: a mapping of long multi-line strings.

For each input, every library decodes the YAML into the same Haskell type and
encodes a value of that type back to YAML. The times below come from GHC
9.10.3 on one machine, on 2026-09-25. The `yaml` package uses the libyaml C
library and converts the data by way of an aeson `Value`.

Decoding:

| Input              | yamlet | HsYAML  | yaml   |
|--------------------|--------|---------|--------|
| `config`, 1105 KiB | 48 ms  | 2948 ms | 117 ms |
| `json`, 432 KiB    | 24 ms  | 2387 ms | 67 ms  |
| `text`, 834 KiB    | 9.7 ms | 617 ms  | 13 ms  |

Encoding:

| Input              | yamlet | HsYAML | yaml   |
|--------------------|--------|--------|--------|
| `config`, 1105 KiB | 38 ms  | 38 ms  | 58 ms  |
| `json`, 432 KiB    | 21 ms  | 19 ms  | 33 ms  |
| `text`, 834 KiB    | 6.2 ms | 10 ms  | 9.5 ms |

## Tests

The test suite reads the data of the YAML test suite from
`tests/yaml-test-suite`. To download it, run this command:

```
scripts/fetch-test-suite.sh
```

The file `tests/error-messages.txt` holds the expected error message for
each invalid input of the YAML test suite. If you change an error message
on purpose, update the file with this command and review the diff:

```
YAMLET_ACCEPT_ERRORS=1 cabal test
```
