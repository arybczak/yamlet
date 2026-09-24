# yamlet

A YAML 1.2.2 library written in Haskell. It depends only on the packages that
come with GHC.

## Features

- The parser follows the grammar of the YAML 1.2.2 specification. It passes
  all 402 cases of the [YAML test suite](https://github.com/yaml/yaml-test-suite)
  (release `data-2022-01-17`), for both the parse events and the JSON values.
- Errors give the line and the column of the problem, both counted from 1, and
  an excerpt of the input. Messages name the kinds of values in plain words,
  e.g. `expected a list, but got an integer`.
- Mappings keep the order of their keys, on input and on output.
- If a string reads back the same as a plain scalar, the encoder does not
  quote it. Thus `dist-newstyle` stays unquoted.
- The input can be UTF-8, UTF-16 or UTF-32. The library detects the encoding
  as the specification describes.

## Modules

- `Yamlet`: decoding with the `FromYAML` class and encoding with the `ToYAML`
  class.
- `Yamlet.Node`: the representation graph, with resolved tags and aliases.
- `Yamlet.Syntax`: the syntax tree, with styles, anchors and unresolved tags.
- `Yamlet.Event`: the parse events of the specification.

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
