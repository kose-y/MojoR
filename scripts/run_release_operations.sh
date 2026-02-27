#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: run_release_operations.sh [options]

Release operations runner:
  1) run release validation
  2) verify generated report consistency
  3) archive report snapshot with UTC timestamp

Options:
  --report <path>          release report path (default: docs/BASELINES/RELEASE_REPORT_LATEST.md)
  --archive-dir <path>     archive directory (default: docs/BASELINES/RELEASE_REPORTS)
  --require-pass           require report Overall PASS
  --allow-mismatch         pass through to release validation runner
  --allow-artifacts        pass through to release validation runner
  --allow-drift            pass through to release validation runner
  --allow-fallback-growth  pass through to release validation runner
  --with-tests             pass through to release validation runner
  --with-perf              pass through to release validation runner (default)
  --without-perf           disable perf guardrail in release validation runner
  --with-gpuarray-f64-routes
                          pass through to release validation runner (default)
  --without-gpuarray-f64-routes
                          disable GPUArray f64 route gate in release validation runner
  --perf-baseline <csv>    pass through to release validation runner
  --perf-current <csv>     pass through to release validation runner
  --gpuarray-f64-baseline <csv>
                          pass through GPUArray f64 route baseline to validation runner
  --gpuarray-f64-repeats <n>
                          pass through GPUArray f64 route repeats to validation runner
  --gpuarray-f64-profile <name>
                          pass through GPUArray f64 route profile to validation runner
  --verbose                verbose output
  --help, -h               show help

Exit codes:
  0 success
  1 failing check
  2 usage error
USAGE
}

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
REPORT="$REPO_ROOT/docs/BASELINES/RELEASE_REPORT_LATEST.md"
ARCHIVE_DIR="$REPO_ROOT/docs/BASELINES/RELEASE_REPORTS"
REQUIRE_PASS=0
VERBOSE=0

ALLOW_MISMATCH=0
ALLOW_ARTIFACTS=0
ALLOW_DRIFT=0
ALLOW_FALLBACK_GROWTH=0
WITH_TESTS=0
WITH_PERF=1
WITH_GPUARRAY_F64_ROUTES=1
PERF_BASELINE=""
PERF_CURRENT=""
GPUARRAY_F64_BASELINE=""
GPUARRAY_F64_REPEATS=""
GPUARRAY_F64_PROFILE=""

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --report) REPORT="${2:-}"; shift 2 ;;
    --archive-dir) ARCHIVE_DIR="${2:-}"; shift 2 ;;
    --require-pass) REQUIRE_PASS=1; shift ;;
    --allow-mismatch) ALLOW_MISMATCH=1; shift ;;
    --allow-artifacts) ALLOW_ARTIFACTS=1; shift ;;
    --allow-drift) ALLOW_DRIFT=1; shift ;;
    --allow-fallback-growth) ALLOW_FALLBACK_GROWTH=1; shift ;;
    --with-tests) WITH_TESTS=1; shift ;;
    --with-perf) WITH_PERF=1; shift ;;
    --without-perf) WITH_PERF=0; shift ;;
    --with-gpuarray-f64-routes) WITH_GPUARRAY_F64_ROUTES=1; shift ;;
    --without-gpuarray-f64-routes) WITH_GPUARRAY_F64_ROUTES=0; shift ;;
    --perf-baseline) PERF_BASELINE="${2:-}"; shift 2 ;;
    --perf-current) PERF_CURRENT="${2:-}"; shift 2 ;;
    --gpuarray-f64-baseline) GPUARRAY_F64_BASELINE="${2:-}"; shift 2 ;;
    --gpuarray-f64-repeats) GPUARRAY_F64_REPEATS="${2:-}"; shift 2 ;;
    --gpuarray-f64-profile) GPUARRAY_F64_PROFILE="${2:-}"; shift 2 ;;
    --verbose) VERBOSE=1; shift ;;
    --help|-h) usage; exit 0 ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

if [ "$WITH_PERF" -eq 1 ] && { [[ -z "$PERF_BASELINE" ]] || [[ -z "$PERF_CURRENT" ]]; }; then
  echo "perf guardrail is enabled by default and requires --perf-baseline and --perf-current." >&2
  exit 2
fi

cd "$REPO_ROOT"

set -- bash "$REPO_ROOT/scripts/run_release_validation.sh" --report "$REPORT"
if [ "$ALLOW_MISMATCH" -eq 1 ]; then set -- "$@" --allow-mismatch; fi
if [ "$ALLOW_ARTIFACTS" -eq 1 ]; then set -- "$@" --allow-artifacts; fi
if [ "$ALLOW_DRIFT" -eq 1 ]; then set -- "$@" --allow-drift; fi
if [ "$ALLOW_FALLBACK_GROWTH" -eq 1 ]; then set -- "$@" --allow-fallback-growth; fi
if [ "$WITH_TESTS" -eq 1 ]; then set -- "$@" --with-tests; fi
if [ "$WITH_PERF" -eq 1 ]; then
  set -- "$@" --with-perf --perf-baseline "$PERF_BASELINE" --perf-current "$PERF_CURRENT"
else
  set -- "$@" --without-perf
fi
if [ "$WITH_GPUARRAY_F64_ROUTES" -eq 1 ]; then
  set -- "$@" --with-gpuarray-f64-routes
  if [ -n "$GPUARRAY_F64_BASELINE" ]; then set -- "$@" --gpuarray-f64-baseline "$GPUARRAY_F64_BASELINE"; fi
  if [ -n "$GPUARRAY_F64_REPEATS" ]; then set -- "$@" --gpuarray-f64-repeats "$GPUARRAY_F64_REPEATS"; fi
  if [ -n "$GPUARRAY_F64_PROFILE" ]; then set -- "$@" --gpuarray-f64-profile "$GPUARRAY_F64_PROFILE"; fi
fi
if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi

if [ "$VERBOSE" -eq 1 ]; then
  echo "[run] release validation runner"
fi
"$@"

set -- bash "$REPO_ROOT/scripts/check_release_report_consistency.sh" --report "$REPORT"
if [ "$REQUIRE_PASS" -eq 1 ]; then set -- "$@" --require-pass; fi
if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi

if [ "$VERBOSE" -eq 1 ]; then
  echo "[run] release report consistency"
fi
"$@"

mkdir -p "$ARCHIVE_DIR"
stamp="$(date -u +%Y%m%dT%H%M%SZ)"
archive_path="$ARCHIVE_DIR/release_report_${stamp}.md"
cp "$REPORT" "$archive_path"

echo "Release operations: complete"
echo "Latest report: $REPORT"
echo "Archived report: $archive_path"
exit 0
