#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_perf_guardrail.sh --baseline <file.csv> --current <file.csv> [--max-regression-ratio <float>] [--verbose]

Compares benchmark timings (ms) against a baseline.

CSV format:
  name,ms
  benchmark_a,1.23
  benchmark_b,4.56

Regression rule:
  current_ms / baseline_ms > max_regression_ratio  => fail

Defaults:
  max_regression_ratio = 1.08

Exit codes:
  0 no regressions
  1 regressions detected
  2 usage/input error
USAGE
}

BASELINE=""
CURRENT=""
MAX_RATIO="1.08"
VERBOSE=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --baseline)
      BASELINE="${2:-}"
      shift 2
      ;;
    --current)
      CURRENT="${2:-}"
      shift 2
      ;;
    --max-regression-ratio)
      MAX_RATIO="${2:-}"
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

if [[ -z "$BASELINE" || -z "$CURRENT" ]]; then
  echo "Both --baseline and --current are required." >&2
  usage
  exit 2
fi

if [[ ! -f "$BASELINE" || ! -f "$CURRENT" ]]; then
  echo "Baseline/current file missing." >&2
  exit 2
fi

if ! awk -v v="$MAX_RATIO" 'BEGIN { exit !(v + 0 > 0) }'; then
  echo "--max-regression-ratio must be a positive number." >&2
  exit 2
fi

if [ "$VERBOSE" -eq 1 ]; then
  echo "Baseline: $BASELINE"
  echo "Current: $CURRENT"
  echo "Max regression ratio: $MAX_RATIO"
fi

tmpfile="$(mktemp)"
trap 'rm -f "$tmpfile"' EXIT

awk -F, -v max_ratio="$MAX_RATIO" '
  NR == FNR {
    if (FNR == 1) next
    name = $1
    val = $2
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", name)
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", val)
    if (name != "" && val != "") {
      base[name] = val + 0.0
    }
    next
  }
  FNR == 1 { next }
  {
    name = $1
    val = $2
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", name)
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", val)
    if (name == "" || val == "") next
    if (!(name in base)) {
      printf("WARN missing baseline: %s\n", name)
      next
    }
    b = base[name]
    c = val + 0.0
    if (b <= 0) {
      printf("WARN non-positive baseline skipped: %s (%f)\n", name, b)
      next
    }
    ratio = c / b
    if (ratio > max_ratio) {
      printf("REGRESSION %s baseline=%f current=%f ratio=%f limit=%f\n", name, b, c, ratio, max_ratio)
      failed = 1
    }
  }
  END {
    if (failed) exit 1
    exit 0
  }
' "$BASELINE" "$CURRENT" > "$tmpfile" || {
  cat "$tmpfile"
  echo "Performance guardrail: regressions detected."
  exit 1
}

cat "$tmpfile"
echo "Performance guardrail: clean"
exit 0
