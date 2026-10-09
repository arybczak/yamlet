# Coming from the yaml package

The [yaml](https://hackage.haskell.org/package/yaml) package decodes and
encodes with the instances of aeson. The instances of yamlet, the generic
ones too, read and write the same YAML as the instances of aeson, so files
written for the yaml package keep working, with the exceptions below.

## Switching in steps

The package [yamlet-aeson](https://hackage.haskell.org/package/yamlet-aeson)
decodes and encodes a type with its instances of aeson, wrapped in
`ViaAeson`, e.g. `decodeFile @(ViaAeson Config)`. A program can switch to
the parser of yamlet first and derive the instances of yamlet later, one
type at a time. A field whose type has only instances of aeson derives its
instances of yamlet via `ViaAeson`.

Through `ViaAeson`, the rules of YAML 1.2 below apply, but the types follow
the instances of aeson, e.g. `1.0` is an `Int`.

## YAML 1.2

yamlet follows YAML 1.2 where the yaml package does not:

- `y`, `yes`, `on`, `n`, `no` and `off` are strings, not booleans. A
  decoder that expects a `Bool` suggests `true` or `false`.
- `.5`, `+.5`, `.inf`, `-.Inf`, `.NaN` and similar values are floats. The
  yaml package reads a number only if a digit comes first, after an
  optional sign, e.g. `1`, `+1`, `007` or `0x1F`. It reads such values as
  strings and writes such strings without quotes, although YAML 1.1 reads
  them as floats too. A yamlet decoder that expects a string suggests
  quotes.
- A scalar with a tag that is not of the core schema is a string, e.g.
  `!secret 123` is the string `123`. The yaml package ignores such a tag
  and reads `123` as a number.
- `<<` is an ordinary key. The yaml package merges the entries of a `<<`
  key into its mapping, as YAML 1.1 does.
- U+2028 and U+2029 in a string are ordinary characters. In a string that
  the yaml package writes in single quotes, e.g. `true` followed by U+2028,
  it writes them as line breaks with indentation after them, so the string
  that yamlet reads back keeps the spaces of the indentation.
- The keys of a mapping must be unique, so two equal keys are an error. The
  yaml package keeps the value of the last one.
- Every line of a flow collection or of a quoted scalar must be indented
  more than the key or the `-` of its entry, the closing bracket too. The
  yaml package also reads lines with less indentation, e.g. a `}` at the
  start of a line:

  ```yaml
  server: {
    port: 80
  }
  ```

## Types

yamlet does not convert values to the types of JSON:

- The keys of a map keep their type, e.g. the keys of a `Map Int` are
  integers. aeson writes every key as a string, so the yaml package writes
  the key `1` as `'1'`, which yamlet does not decode as an `Int`. The other
  way round, the yaml package reads every key as a string, e.g. the key
  `404` or `true` of a `Map Text`. yamlet rejects such a key without
  quotes.
- An `IntMap` and a map with keys that aeson cannot write as strings, e.g.
  a `Map (Int, Int)`, are mappings in yamlet. aeson writes them as lists of
  pairs.
- An infinite `Double` is `.inf` or `-.inf`. aeson writes the string `+inf`
  or `-inf`, because JSON has no infinity, and yamlet reads these as
  strings.
- A NaN `Double` is `.nan`. aeson writes `null`, which yamlet rejects for a
  `Double`. The yaml package reads `.nan` as a string, so it does not decode
  it as a `Double`.
- A float with an exponent beyond the range from -1000 to 1000 is an error,
  e.g. `1e1001`, which keeps the decoding of untrusted input fast. The yaml
  package reads `1e1001` as an infinite `Double`.
- A value must have the YAML type of its Haskell type. yamlet rejects some
  values that aeson converts, e.g. `1.0` for an `Int`, `0.5` for a
  `Rational` and `null` for a `Double`.
- The elements of a `Set` or an `IntSet` must be unique, so `[a, a]` is an
  error. aeson keeps one of them.
- A `Fixed` value must be a multiple of the resolution of its type, so
  `1.255` is an error for a `Centi`. aeson rounds it down to `1.25`.
- The mapping of a `Rational`, a `CalendarDiffDays` or a
  `CalendarDiffTime` must have only the keys of the type, e.g. `numerator`
  and `denominator`. aeson ignores other keys.
- `Proxy` has no instances, because it holds no value. aeson writes it as
  `null` and reads any value as a `Proxy`.

## Generic instances

- A key that is not a field of the constructor is an error. aeson ignores
  such a key. To ignore it in yamlet too, turn off the option
  `rejectUnknownFields`.
- A type with one constructor without fields is the name of the
  constructor, e.g. `Unit`. aeson writes `[]`.
- With the encoding `SingleField`, a constructor without fields is its
  name, e.g. `Dot`. aeson writes `{Dot: []}`.
- A constructor with several fields without names is a compile error.
  aeson writes the fields as a list. Give the fields names.
- A type cannot mix a constructor with named fields and a constructor with
  one field without a name, except with the encoding `SingleField`. Such a
  type is a compile error. aeson writes the field without a name under the
  key `contents`.

The documentation of the instances in
[Yamlet.Decode](https://hackage.haskell.org/package/yamlet-1.0.0.0/candidate/docs/Yamlet-Decode.html)
and [Yamlet.Encode](https://hackage.haskell.org/package/yamlet-1.0.0.0/candidate/docs/Yamlet-Encode.html),
and of the options in
[Yamlet.Generic](https://hackage.haskell.org/package/yamlet-1.0.0.0/candidate/docs/Yamlet-Generic.html),
describes the remaining details.
