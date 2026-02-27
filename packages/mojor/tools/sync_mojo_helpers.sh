#!/bin/sh
set -eu

ROOT="$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)"
SRC_DIR="$ROOT/src"
DST_DIR="$ROOT/inst/mojo_helpers"
HELPERS="abi_types.mojo debug_helpers.mojo na_helpers.mojo quantile_helpers.mojo rng_helpers.mojo set_match_helpers.mojo ziggurat_constants.mojo"

mkdir -p "$DST_DIR"
find "$DST_DIR" -maxdepth 1 -type f -name '*.mojo' -exec rm -f {} +

for helper in $HELPERS; do
  src="$SRC_DIR/$helper"
  dst="$DST_DIR/$helper"
  if [ ! -f "$src" ]; then
    echo "sync_mojo_helpers: missing helper source: $src" >&2
    exit 1
  fi
  cp "$src" "$dst"
done

echo "sync_mojo_helpers: synced helper files to $DST_DIR"
