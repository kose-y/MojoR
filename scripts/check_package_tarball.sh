#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_package_tarball.sh [options]

Builds the package tarball and runs R CMD check against it.

Options:
  --package-dir <path>   Package directory (default: packages/mojor)
  --artifact-dir <path>  Directory for logs/artifacts (default: .artifacts/package-check)
  --verbose              Print executed commands.
  --help, -h             Show this help text.

Exit codes:
  0 package tarball check passed
  1 package tarball check failed
  2 usage error
USAGE
}

PACKAGE_DIR="packages/mojor"
ARTIFACT_DIR=".artifacts/package-check"
VERBOSE=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --package-dir)
      [ "${2:-}" != "" ] || { echo "Missing value for --package-dir" >&2; usage; exit 2; }
      PACKAGE_DIR="$2"
      shift 2
      ;;
    --artifact-dir)
      [ "${2:-}" != "" ] || { echo "Missing value for --artifact-dir" >&2; usage; exit 2; }
      ARTIFACT_DIR="$2"
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

PACKAGE_DIR_ABS="$REPO_ROOT/$PACKAGE_DIR"
ARTIFACT_DIR_ABS="$REPO_ROOT/$ARTIFACT_DIR"
BACKEND_SYMBOL_SCRIPT="$REPO_ROOT/scripts/check_backend_bundle_symbols.sh"

if [ ! -d "$PACKAGE_DIR_ABS" ]; then
  echo "Package directory not found: $PACKAGE_DIR_ABS" >&2
  exit 1
fi
if [ ! -x "$BACKEND_SYMBOL_SCRIPT" ]; then
  echo "Backend symbol script missing or not executable: $BACKEND_SYMBOL_SCRIPT" >&2
  exit 1
fi

if ! command -v R >/dev/null 2>&1; then
  echo "R binary not found on PATH." >&2
  exit 1
fi

DESCRIPTION_FILE="$PACKAGE_DIR_ABS/DESCRIPTION"
if [ ! -f "$DESCRIPTION_FILE" ]; then
  echo "DESCRIPTION not found: $DESCRIPTION_FILE" >&2
  exit 1
fi

PKG_NAME="$(awk -F': *' '/^Package:/ {print $2; exit}' "$DESCRIPTION_FILE")"
PKG_VERSION="$(awk -F': *' '/^Version:/ {print $2; exit}' "$DESCRIPTION_FILE")"
if [ -z "$PKG_NAME" ] || [ -z "$PKG_VERSION" ]; then
  echo "Unable to determine package name/version from $DESCRIPTION_FILE" >&2
  exit 1
fi

TARBALL="$REPO_ROOT/${PKG_NAME}_${PKG_VERSION}.tar.gz"
RCHECK_DIR="$REPO_ROOT/${PKG_NAME}.Rcheck"

mkdir -p "$ARTIFACT_DIR_ABS"

collect_rcheck_logs() {
  local src dst
  src="$RCHECK_DIR/00check.log"
  dst="$ARTIFACT_DIR_ABS/00check.log"
  if [ -f "$src" ]; then
    cp "$src" "$dst"
  fi
  src="$RCHECK_DIR/00install.out"
  dst="$ARTIFACT_DIR_ABS/00install.out"
  if [ -f "$src" ]; then
    cp "$src" "$dst"
  fi
}
trap collect_rcheck_logs EXIT

run_cmd() {
  local label="$1"
  local log_file="$2"
  shift 2

  echo "[run] $label"
  if [ "$VERBOSE" -eq 1 ]; then
    echo "      $*"
  fi

  set +e
  "$@" > >(tee "$log_file") 2>&1
  local rc=${PIPESTATUS[0]}
  set -e

  if [ "$rc" -ne 0 ]; then
    echo "[err] $label"
    return "$rc"
  fi
  echo "[ok ] $label"
  return 0
}

is_truthy_env() {
  local value="${1:-}"
  value="$(printf '%s' "$value" | tr '[:upper:]' '[:lower:]')"
  case "$value" in
    1|true|yes|y|on) return 0 ;;
    *) return 1 ;;
  esac
}

export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_COLLATE=C
export LC_MESSAGES=C
export LC_MONETARY=C
export LC_NUMERIC=C
export LC_TIME=C

# Keep test subprocesses from rebuilding backend during R CMD check.
# In-check builds can race with installed bridge state and cause instability.
export MOJOR_TEST_SKIP_BUILD=1
# Tarball CI lanes are CPU-only; avoid runtime GPU probing crashes in tests.
export MOJOR_TEST_ASSUME_NO_GPU="${MOJOR_TEST_ASSUME_NO_GPU:-1}"
export MOJOR_TEST_SUBPROCESS_LOG_DIR="${MOJOR_TEST_SUBPROCESS_LOG_DIR:-$ARTIFACT_DIR_ABS/subprocess-logs}"
mkdir -p "$MOJOR_TEST_SUBPROCESS_LOG_DIR"

# Release package-check profile defaults: keep GPU runtime lanes opt-in.
export MOJOR_TEST_RUN_GPU_RUNTIME_TESTS="${MOJOR_TEST_RUN_GPU_RUNTIME_TESTS:-0}"
export MOJOR_TEST_RUN_GPU_BACKEND_TESTS="${MOJOR_TEST_RUN_GPU_BACKEND_TESTS:-0}"
export MOJOR_TEST_RUN_GPU_JIT_PREVIEW_RUNTIME="${MOJOR_TEST_RUN_GPU_JIT_PREVIEW_RUNTIME:-0}"
export MOJOR_TEST_GPU_JIT_AUTO_MATRIX2D="${MOJOR_TEST_GPU_JIT_AUTO_MATRIX2D:-0}"
export MOJOR_TEST_GPU_STRICT_BIND="${MOJOR_TEST_GPU_STRICT_BIND:-0}"
export MOJOR_TEST_GPU_CTX_SMOKE="${MOJOR_TEST_GPU_CTX_SMOKE:-0}"
export MOJOR_TEST_GPU_EW="${MOJOR_TEST_GPU_EW:-0}"
SKIP_BACKEND_BUNDLE_SYMBOL_CHECK="${MOJOR_SKIP_BACKEND_BUNDLE_SYMBOL_CHECK:-0}"
# Use an isolated per-run cache root to avoid stale local cache artifacts from
# affecting release package-check outcomes.
export MOJOR_CACHE_DIR="${MOJOR_CACHE_DIR:-$ARTIFACT_DIR_ABS/mojor-cache}"
if [ "${MOJOR_CHECK_REUSE_CACHE:-0}" != "1" ]; then
  rm -rf "$MOJOR_CACHE_DIR"
fi
mkdir -p "$MOJOR_CACHE_DIR"

rm -f "$TARBALL"
rm -rf "$RCHECK_DIR"

case "$(printf '%s' "$SKIP_BACKEND_BUNDLE_SYMBOL_CHECK" | tr '[:upper:]' '[:lower:]')" in
  1|true|yes|on)
    echo "[skip] backend bundle symbols (MOJOR_SKIP_BACKEND_BUNDLE_SYMBOL_CHECK=$SKIP_BACKEND_BUNDLE_SYMBOL_CHECK)"
    ;;
  *)
    if [ "$VERBOSE" -eq 1 ]; then
      run_cmd "backend bundle symbols" \
        "$ARTIFACT_DIR_ABS/backend_symbols.out" \
        bash "$BACKEND_SYMBOL_SCRIPT" --package-dir "$PACKAGE_DIR_ABS" --verbose
    else
      run_cmd "backend bundle symbols" \
        "$ARTIFACT_DIR_ABS/backend_symbols.out" \
        bash "$BACKEND_SYMBOL_SCRIPT" --package-dir "$PACKAGE_DIR_ABS"
    fi
    ;;
esac

run_cmd "R CMD build $PACKAGE_DIR" \
  "$ARTIFACT_DIR_ABS/build.out" \
  R CMD build "$PACKAGE_DIR_ABS"

if [ ! -f "$TARBALL" ]; then
  echo "Tarball not found after build: $TARBALL" >&2
  exit 1
fi

CHECK_ARGS=(--no-manual)
if is_truthy_env "${MOJOR_CHECK_NO_TESTS:-0}"; then
  CHECK_ARGS+=(--no-tests)
fi

run_cmd "R CMD check ${CHECK_ARGS[*]} $(basename "$TARBALL")" \
  "$ARTIFACT_DIR_ABS/check.out" \
  R CMD check "${CHECK_ARGS[@]}" "$TARBALL"

echo "Package tarball check: clean"
echo "Artifacts: $ARTIFACT_DIR_ABS"
exit 0
