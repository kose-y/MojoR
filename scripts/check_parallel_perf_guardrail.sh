#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_parallel_perf_guardrail.sh --baseline <file.csv> --current <file.csv> [--min-speedup-ratio <float>] [--absolute-min-speedup <float>] [--verbose]

Compares parallel speedup metric against baseline.

Expected CSV format:
  name,ms
  mojor_parallel_speedup,1.08

Rules:
  current_speedup / baseline_speedup >= min_speedup_ratio
  current_speedup >= absolute_min_speedup

Defaults:
  min_speedup_ratio = 0.90
  absolute_min_speedup = 1.01

Exit codes:
  0 guardrail clean
  1 regression detected
  2 usage/input error
USAGE
}

BASELINE=""
CURRENT=""
MIN_SPEEDUP_RATIO="0.90"
ABSOLUTE_MIN_SPEEDUP="1.01"
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
    --min-speedup-ratio)
      MIN_SPEEDUP_RATIO="${2:-}"
      shift 2
      ;;
    --absolute-min-speedup)
      ABSOLUTE_MIN_SPEEDUP="${2:-}"
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

if ! awk -v v="$MIN_SPEEDUP_RATIO" 'BEGIN { exit !(v + 0 > 0) }'; then
  echo "--min-speedup-ratio must be a positive number." >&2
  exit 2
fi

if ! awk -v v="$ABSOLUTE_MIN_SPEEDUP" 'BEGIN { exit !(v + 0 > 0) }'; then
  echo "--absolute-min-speedup must be a positive number." >&2
  exit 2
fi

if [ "$VERBOSE" -eq 1 ]; then
  echo "Baseline: $BASELINE"
  echo "Current: $CURRENT"
  echo "Min speedup ratio: $MIN_SPEEDUP_RATIO"
  echo "Absolute min speedup: $ABSOLUTE_MIN_SPEEDUP"
fi

tmpfile="$(mktemp)"
trap 'rm -f "$tmpfile"' EXIT

awk -F, -v min_ratio="$MIN_SPEEDUP_RATIO" -v abs_min="$ABSOLUTE_MIN_SPEEDUP" '
  NR == FNR {
    if (FNR == 1) next
    name = $1
    val = $2
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", name)
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", val)
    if (name == "mojor_parallel_speedup" && val != "") {
      base = val + 0.0
      have_base = 1
    }
    next
  }
  FNR == 1 { next }
  {
    name = $1
    val = $2
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", name)
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", val)
    if (name == "mojor_parallel_speedup" && val != "") {
      cur = val + 0.0
      have_cur = 1
    }
  }
  END {
    failed = 0
    if (!have_base) {
      print "ERROR missing baseline metric: mojor_parallel_speedup"
      exit 2
    }
    if (!have_cur) {
      print "ERROR missing current metric: mojor_parallel_speedup"
      exit 2
    }
    if (base <= 0) {
      print "ERROR baseline speedup must be > 0"
      exit 2
    }

    ratio = cur / base
    printf("parallel speedup baseline=%f current=%f ratio=%f min_ratio=%f abs_min=%f\n", base, cur, ratio, min_ratio, abs_min)

    if (ratio < min_ratio) {
      print "REGRESSION speedup ratio below minimum threshold"
      failed = 1
    }
    if (cur < abs_min) {
      print "REGRESSION current speedup below absolute minimum"
      failed = 1
    }

    if (failed) exit 1
    exit 0
  }
' "$BASELINE" "$CURRENT" > "$tmpfile" || {
  rc=$?
  cat "$tmpfile"
  if [ "$rc" -eq 2 ]; then
    echo "Parallel performance guardrail: input error."
    exit 2
  fi
  echo "Parallel performance guardrail: regressions detected."
  exit 1
}

cat "$tmpfile"
echo "Parallel performance guardrail: clean"
exit 0
