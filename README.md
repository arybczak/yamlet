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

The benchmark in `bench/` parses three generated inputs. The times below come
from GHC 9.14 on one machine. The `yaml` package uses the libyaml C library and
converts the result to an aeson `Value`.

| Input                   | yamlet (nodes)  | HsYAML (nodes) | yaml   |
|-------------------------|-----------------|----------------|--------|
| records, 1105 KiB       | 32 ms           | 2970 ms        | 116 ms |
| flow collections, 432 KiB | 18 ms         | 2370 ms        | 56 ms  |
| block scalars, 834 KiB  | 5.6 ms          | 611 ms         | 13 ms  |

## Tests

The test suite reads the data of the YAML test suite from
`tests/yaml-test-suite`. To download it, run this command:

```
scripts/fetch-test-suite.sh
```
