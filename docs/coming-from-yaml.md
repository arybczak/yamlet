# Coming from the yaml package

The [yaml](https://hackage.haskell.org/package/yaml) package decodes and
encodes with the instances of aeson. The instances of yamlet, the generic
ones too, read and write the same YAML as the instances of aeson, so files
written for the yaml package keep working, with the exceptions below.

## YAML 1.2

yamlet follows YAML 1.2 where the yaml package does not:

- `y`, `yes`, `on`, `n`, `no` and `off` are strings, not booleans. A
  decoder that expects a `Bool` suggests `true` or `false`.
- `.5`, `+.5`, `.inf`, `-.Inf`, `.NaN` and similar values are floats. The
  yaml package reads a number only in the syntax of JSON, apart from the
  `0x` and `0o` prefixes. It reads such values as strings and writes such
  strings without quotes, although YAML 1.1 reads them as floats too. A
  yamlet decoder that expects a string suggests quotes.
- `<<` is an ordinary key. The yaml package merges the entries of a `<<`
  key into its mapping, as YAML 1.1 does.
- U+2028 and U+2029 in a string are ordinary characters. The yaml package
  writes them as line breaks with indentation after them, so the string
  that yamlet reads back keeps the spaces of the indentation.
- The keys of a mapping must be unique, so two equal keys are an error. The
  yaml package keeps the value of the last one.

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

The documentation of the instances in
[Yamlet.Decode](https://hackage.haskell.org/package/yamlet-1.0.0.0/candidate/docs/Yamlet-Decode.html)
and [Yamlet.Encode](https://hackage.haskell.org/package/yamlet-1.0.0.0/candidate/docs/Yamlet-Encode.html),
and of the options in
[Yamlet.Generic](https://hackage.haskell.org/package/yamlet-1.0.0.0/candidate/docs/Yamlet-Generic.html),
describes the remaining details.
