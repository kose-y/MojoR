#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_gibbs_perf_guardrail.sh --baseline <file.csv> --current <file.csv> [--max-build-regression-ratio <float>] [--max-runtime-regression-ratio <float>] [--min-speedup <float>] [--verbose]

Compares Gibbs benchmark metrics (name,ms CSV) against a baseline.

Required metrics:
  gibbs_r_mean_ms
  gibbs_mojor_build_ms
  gibbs_mojor_mean_ms
  gibbs_mojor_fast_build_ms
  gibbs_mojor_fast_mean_ms

Optional metrics (checked only when present in both files):
  gibbs_bulk_build_ms
  gibbs_bulk_mean_ms

Rules:
  build metric ratio   = current / baseline <= max_build_regression_ratio
  runtime metric ratio = current / baseline <= max_runtime_regression_ratio
  current speedup      = gibbs_r_mean_ms / gibbs_mojor_mean_ms >= min_speedup

Defaults:
  max_build_regression_ratio = 1.35
  max_runtime_regression_ratio = 1.20
  min_speedup = 2.00

Exit codes:
  0 guardrail clean
  1 regressions detected
  2 usage/input error
USAGE
}

BASELINE=""
CURRENT=""
MAX_BUILD_RATIO="1.35"
MAX_RUNTIME_RATIO="1.20"
MIN_SPEEDUP="2.00"
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
    --max-build-regression-ratio)
      MAX_BUILD_RATIO="${2:-}"
      shift 2
      ;;
    --max-runtime-regression-ratio)
      MAX_RUNTIME_RATIO="${2:-}"
      shift 2
      ;;
    --min-speedup)
      MIN_SPEEDUP="${2:-}"
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

for v in "$MAX_BUILD_RATIO" "$MAX_RUNTIME_RATIO" "$MIN_SPEEDUP"; do
  if ! awk -v x="$v" 'BEGIN { exit !(x + 0 > 0) }'; then
    echo "Threshold values must be positive numbers." >&2
    exit 2
  fi
done

if [ "$VERBOSE" -eq 1 ]; then
  echo "Baseline: $BASELINE"
  echo "Current: $CURRENT"
  echo "Max build regression ratio: $MAX_BUILD_RATIO"
  echo "Max runtime regression ratio: $MAX_RUNTIME_RATIO"
  echo "Min speedup: $MIN_SPEEDUP"
fi

tmpfile="$(mktemp)"
trap 'rm -f "$tmpfile"' EXIT

awk -F, \
  -v max_build="$MAX_BUILD_RATIO" \
  -v max_runtime="$MAX_RUNTIME_RATIO" \
  -v min_speedup="$MIN_SPEEDUP" '
  function trim(s) {
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", s)
    return s
  }
  function check_ratio(metric, limit, klass, b, c, ratio) {
    if (!(metric in base) || !(metric in cur)) return
    b = base[metric]
    c = cur[metric]
    if (b <= 0) {
      printf("ERROR baseline metric must be > 0: %s=%f\n", metric, b)
      input_err = 1
      return
    }
    ratio = c / b
    printf("%s baseline=%f current=%f ratio=%f limit=%f class=%s\n", metric, b, c, ratio, limit, klass)
    if (ratio > limit) {
      printf("REGRESSION %s ratio %f exceeds limit %f\n", metric, ratio, limit)
      failed = 1
    }
  }
  NR == FNR {
    if (FNR == 1) next
    name = trim($1)
    val = trim($2)
    if (name != "" && val != "") base[name] = val + 0.0
    next
  }
  FNR == 1 { next }
  {
    name = trim($1)
    val = trim($2)
    if (name != "" && val != "") cur[name] = val + 0.0
  }
  END {
    req[1] = "gibbs_r_mean_ms"
    req[2] = "gibbs_mojor_build_ms"
    req[3] = "gibbs_mojor_mean_ms"
    req[4] = "gibbs_mojor_fast_build_ms"
    req[5] = "gibbs_mojor_fast_mean_ms"

    for (i = 1; i <= 5; i++) {
      m = req[i]
      if (!(m in base)) {
        printf("ERROR missing baseline metric: %s\n", m)
        input_err = 1
      }
      if (!(m in cur)) {
        printf("ERROR missing current metric: %s\n", m)
        input_err = 1
      }
    }

    if (input_err) exit 2

    check_ratio("gibbs_mojor_build_ms", max_build, "build")
    check_ratio("gibbs_mojor_fast_build_ms", max_build, "build")
    check_ratio("gibbs_mojor_mean_ms", max_runtime, "runtime")
    check_ratio("gibbs_mojor_fast_mean_ms", max_runtime, "runtime")

    if (("gibbs_bulk_build_ms" in base) && ("gibbs_bulk_build_ms" in cur)) {
      check_ratio("gibbs_bulk_build_ms", max_build, "build")
    } else if (("gibbs_bulk_build_ms" in base) || ("gibbs_bulk_build_ms" in cur)) {
      print "WARN bulk build metric present in only one file; skipping"
    }

    if (("gibbs_bulk_mean_ms" in base) && ("gibbs_bulk_mean_ms" in cur)) {
      check_ratio("gibbs_bulk_mean_ms", max_runtime, "runtime")
    } else if (("gibbs_bulk_mean_ms" in base) || ("gibbs_bulk_mean_ms" in cur)) {
      print "WARN bulk mean metric present in only one file; skipping"
    }

    if (cur["gibbs_mojor_mean_ms"] <= 0) {
      print "ERROR current gibbs_mojor_mean_ms must be > 0"
      input_err = 1
    } else {
      speedup = cur["gibbs_r_mean_ms"] / cur["gibbs_mojor_mean_ms"]
      printf("gibbs_speedup_current=%f min_required=%f\n", speedup, min_speedup)
      if (speedup < min_speedup) {
        printf("REGRESSION speedup %f is below minimum %f\n", speedup, min_speedup)
        failed = 1
      }
    }

    if (input_err) exit 2
    if (failed) exit 1
    exit 0
  }
' "$BASELINE" "$CURRENT" > "$tmpfile" || {
  rc=$?
  cat "$tmpfile"
  if [ "$rc" -eq 2 ]; then
    echo "Gibbs performance guardrail: input error."
    exit 2
  fi
  echo "Gibbs performance guardrail: regressions detected."
  exit 1
}

cat "$tmpfile"
echo "Gibbs performance guardrail: clean"
exit 0
