#!/usr/bin/env bash
# Run the benchmarks of the performance section of the README and print its
# tables. Each benchmark runs once in its own process, pinned to one core,
# because the earlier benchmarks of a process slow down the later ones.
#
# The core is 2 by default, which is on the CCD with the 3D V-cache of the
# Ryzen 9950X3D that the README names. Set CORE to use another core.
set -euo pipefail

cd "$(dirname "$0")/.."
core=${CORE:-2}
inputs=(config json text)
libraries=(yamlet HsYAML yaml)

cabal build -v0 yamlet:bench
bench=$(cabal list-bin yamlet:bench)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

for op in decode encode; do
  for input in "${inputs[@]}"; do
    for library in "${libraries[@]}"; do
      name="All.$op.$input.$library"
      echo "$name" >&2
      taskset -c "$core" "$bench" -p "\$0 == \"$name\"" --csv "$out/$name.csv" > "$out/sizes"
    done
  done
done

# The time in milliseconds from the mean in picoseconds of a CSV file of
# tasty-bench, with one decimal below 10 ms.
milliseconds() {
  awk -F, 'NR == 2 { ms = $2 / 1e9; printf(ms < 10 ? "%.1f ms" : "%.0f ms", ms) }' "$1"
}

# The benchmark prints the size of each input, e.g. "config: 1105 KiB".
size() {
  awk -v input="$1" '$1 == input ":" { print $2 " " $3 }' "$out/sizes"
}

table() {
  local op=$1
  {
    printf 'Input'
    printf '\t%s' "${libraries[@]}"
    printf '\n'
    for input in "${inputs[@]}"; do
      printf '`%s`, %s' "$input" "$(size "$input")"
      for library in "${libraries[@]}"; do
        printf '\t%s' "$(milliseconds "$out/All.$op.$input.$library.csv")"
      done
      printf '\n'
    done
  } | awk -F'\t' '
    { for (i = 1; i <= NF; i++) { cell[NR, i] = $i; if (length($i) > width[i]) width[i] = length($i) } }
    END {
      for (r = 1; r <= NR; r++) {
        line = "|"
        for (i = 1; i <= NF; i++) line = line sprintf(" %-*s |", width[i], cell[r, i])
        print line
        if (r == 1) {
          line = "|"
          for (i = 1; i <= NF; i++) { dashes = sprintf("%*s", width[i] + 2, ""); gsub(/ /, "-", dashes); line = line dashes "|" }
          print line
        }
      }
    }'
}

echo "Decoding:"
echo
table decode
echo
echo "Encoding:"
echo
table encode
