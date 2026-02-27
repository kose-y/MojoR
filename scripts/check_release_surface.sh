#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_release_surface.sh [--allow-drift] [--verbose] [--help]

Checks release surface stability by verifying:
  1) package exports match frozen baseline in docs/BASELINES/RELEASE_SURFACE_BASELINE.md (when present)
  2) package roxygen @export lines match NAMESPACE exports
  3) compatibility metadata wiring is present in package transpile output

Exit codes:
  0 clean
  1 drift detected
  2 usage error
USAGE
}

ALLOW_DRIFT=0
VERBOSE=0
while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --allow-drift) ALLOW_DRIFT=1; shift ;;
    --verbose) VERBOSE=1; shift ;;
    --help|-h) usage; exit 0 ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

BASELINE_FILE="$REPO_ROOT/docs/BASELINES/RELEASE_SURFACE_BASELINE.md"
NAMESPACE_FILE="$REPO_ROOT/packages/mojor/NAMESPACE"
ROXYGEN_FILE="$REPO_ROOT/packages/mojor/R/roxygen_api_docs.R"
PKG_CORE="$REPO_ROOT/packages/mojor/R/core/core.R"
PKG_OUT="$REPO_ROOT/packages/mojor/R/transpile/output.R"

BASELINE_PRESENT=0
if [ -f "$BASELINE_FILE" ]; then
  BASELINE_PRESENT=1
fi

if [ ! -f "$NAMESPACE_FILE" ] || [ ! -f "$ROXYGEN_FILE" ]; then
  echo "Release surface check: required files are missing." >&2
  exit 2
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

if [ "$BASELINE_PRESENT" -eq 1 ]; then
  awk '
    /mojor-export-baseline:start/ { in_block=1; next }
    /mojor-export-baseline:end/ { in_block=0; next }
    in_block {
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", $0)
      if ($0 != "") print $0
    }
  ' "$BASELINE_FILE" | sort -u > "$tmpdir/baseline.txt"
else
  : > "$tmpdir/baseline.txt"
fi

sed -n 's/^export(\([^)]*\))$/\1/p' "$NAMESPACE_FILE" | sort -u > "$tmpdir/namespace.txt"
sed -n "s/^#' @export[[:space:]]\{1,\}\([^[:space:]]\{1,\}\).*$/\1/p" "$ROXYGEN_FILE" | sort -u > "$tmpdir/roxygen.txt"

violation=0

baseline_missing_in_namespace="$(comm -23 "$tmpdir/baseline.txt" "$tmpdir/namespace.txt" || true)"
namespace_extra_vs_baseline="$(comm -13 "$tmpdir/baseline.txt" "$tmpdir/namespace.txt" || true)"
namespace_missing_in_roxygen="$(comm -23 "$tmpdir/namespace.txt" "$tmpdir/roxygen.txt" || true)"
roxygen_extra_vs_namespace="$(comm -13 "$tmpdir/namespace.txt" "$tmpdir/roxygen.txt" || true)"

if [ "$BASELINE_PRESENT" -eq 1 ] && { [[ -n "$baseline_missing_in_namespace" ]] || [[ -n "$namespace_extra_vs_baseline" ]]; }; then
  violation=1
  echo "Release surface drift: baseline vs NAMESPACE mismatch"
  if [[ -n "$baseline_missing_in_namespace" ]]; then
    echo "  Missing in NAMESPACE:"
    printf '%s\n' "$baseline_missing_in_namespace" | sed 's/^/    - /'
  fi
  if [[ -n "$namespace_extra_vs_baseline" ]]; then
    echo "  Extra in NAMESPACE (not in baseline):"
    printf '%s\n' "$namespace_extra_vs_baseline" | sed 's/^/    - /'
  fi
fi

if [[ -n "$namespace_missing_in_roxygen" ]] || [[ -n "$roxygen_extra_vs_namespace" ]]; then
  violation=1
  echo "Release surface drift: NAMESPACE vs roxygen @export mismatch"
  if [[ -n "$namespace_missing_in_roxygen" ]]; then
    echo "  Missing in roxygen @export:"
    printf '%s\n' "$namespace_missing_in_roxygen" | sed 's/^/    - /'
  fi
  if [[ -n "$roxygen_extra_vs_namespace" ]]; then
    echo "  Extra in roxygen @export (not in NAMESPACE):"
    printf '%s\n' "$roxygen_extra_vs_namespace" | sed 's/^/    - /'
  fi
fi

if ! grep -Fq '.mojor_state$api_surface_version <- "2026-Q1"' "$PKG_CORE"; then
  violation=1
  echo "Release surface drift: missing api_surface_version in package core state"
fi
if ! grep -Fq 'compatibility = list(' "$PKG_OUT"; then
  violation=1
  echo "Release surface drift: missing compatibility metadata payload in package transpile output"
fi

if [ "$VERBOSE" -eq 1 ]; then
  if [ "$BASELINE_PRESENT" -eq 1 ]; then
    echo "Baseline exports:"
    sed 's/^/  - /' "$tmpdir/baseline.txt"
  else
    echo "Baseline exports: skipped (missing docs/BASELINES/RELEASE_SURFACE_BASELINE.md)"
  fi
  echo "NAMESPACE exports:"
  sed 's/^/  - /' "$tmpdir/namespace.txt"
  echo "Roxygen exports:"
  sed 's/^/  - /' "$tmpdir/roxygen.txt"
fi

if [ "$violation" -ne 0 ]; then
  if [ "$ALLOW_DRIFT" -eq 1 ]; then
    echo "Continuing despite release-surface drift (--allow-drift)."
    exit 0
  fi
  exit 1
fi

echo "Release surface check: clean"
exit 0
