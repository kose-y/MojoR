#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_backend_bundle_symbols.sh [options]

Validates required backend symbols in the platform bundle under:
  <package-dir>/inst/backend/<platform>/libmojor_backend.<ext>

Options:
  --package-dir <path>  Package directory (default: packages/mojor)
  --verbose             Print resolved paths and platform details.
  --help, -h            Show this help text.

Exit codes:
  0 clean
  1 contract violation (missing file/symbols)
  2 usage/input/tooling error
USAGE
}

PACKAGE_DIR="packages/mojor"
VERBOSE=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --package-dir)
      [ "${2:-}" != "" ] || { echo "Missing value for --package-dir" >&2; usage; exit 2; }
      PACKAGE_DIR="$2"
      shift 2
      ;;
    --verbose)
      VERBOSE=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

if [[ "$PACKAGE_DIR" = /* ]]; then
  PACKAGE_DIR_ABS="$PACKAGE_DIR"
else
  PACKAGE_DIR_ABS="$REPO_ROOT/$PACKAGE_DIR"
fi

if [ ! -d "$PACKAGE_DIR_ABS" ]; then
  echo "Backend bundle symbol check: package directory not found: $PACKAGE_DIR_ABS" >&2
  exit 2
fi

if ! command -v nm >/dev/null 2>&1; then
  echo "Backend bundle symbol check: required tool not found: nm" >&2
  exit 2
fi

OS="$(uname -s)"
ARCH="$(uname -m)"
case "$ARCH" in
  aarch64) ARCH="arm64" ;;
  amd64) ARCH="x86_64" ;;
esac

case "$OS" in
  Darwin)
    EXT="dylib"
    PLATFORM_KEY="darwin-$ARCH"
    NM_CMD=(nm -gU)
    ;;
  Linux)
    EXT="so"
    PLATFORM_KEY="linux-$ARCH"
    NM_CMD=(nm -D --defined-only)
    ;;
  *)
    echo "Backend bundle symbol check: unsupported platform '$OS/$ARCH'" >&2
    exit 2
    ;;
esac

BUNDLE_DIR="$PACKAGE_DIR_ABS/inst/backend/$PLATFORM_KEY"
LIB_PATH="$BUNDLE_DIR/libmojor_backend.$EXT"

if [ "$VERBOSE" -eq 1 ]; then
  echo "Backend bundle symbol check: platform=$PLATFORM_KEY"
  echo "Backend bundle symbol check: library=$LIB_PATH"
fi

if [ ! -f "$LIB_PATH" ]; then
  echo "Backend bundle symbol check: missing backend library: $LIB_PATH"
  exit 1
fi

set +e
NM_OUTPUT="$("${NM_CMD[@]}" "$LIB_PATH" 2>/dev/null)"
NM_RC=$?
set -e

if [ "$NM_RC" -ne 0 ] || [ -z "$NM_OUTPUT" ]; then
  if [ "$OS" = "Linux" ]; then
    set +e
    NM_OUTPUT="$(nm -g "$LIB_PATH" 2>/dev/null)"
    NM_RC=$?
    set -e
  fi
fi

if [ "$NM_RC" -ne 0 ] || [ -z "$NM_OUTPUT" ]; then
  echo "Backend bundle symbol check: failed to read exported symbols from $LIB_PATH" >&2
  exit 2
fi

REQUIRED_SYMBOLS=(
  "mojor_gpu_cap_f64_matmul"
  "mojor_gpu_cap_f64_reduce"
  "mojor_gpu_cap_i32_scatter"
)

missing=()
for sym in "${REQUIRED_SYMBOLS[@]}"; do
  if ! grep -Eq "[[:space:]]_?${sym}$" <<<"$NM_OUTPUT"; then
    missing+=("$sym")
  fi
done

if [ "${#missing[@]}" -ne 0 ]; then
  echo "Backend bundle symbol check: required symbol(s) missing in $(basename "$LIB_PATH"):"
  for sym in "${missing[@]}"; do
    echo "  - $sym"
  done
  exit 1
fi

echo "Backend bundle symbol check: clean"
exit 0
