#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: run_release_validation.sh [options]

Release validation orchestrator:
  1) release readiness check
  2) release-candidate gate run
  3) markdown report generation

Options:
  --report <path>          report output path
  --profile <legacy|release-freeze>
                          release-candidate gate profile (default: legacy)
  --allow-mismatch         pass through to release-candidate gate
  --allow-artifacts        pass through to release-candidate gate
  --allow-drift            pass through to release-candidate gate
  --allow-fallback-growth  pass through to release-candidate gate
  --with-tests             pass through to release-candidate gate
  --with-perf              pass through to release-candidate gate (default)
  --without-perf           disable perf guardrail in release-candidate gate
  --perf-baseline <csv>    pass through to release-candidate gate
  --perf-current <csv>     pass through to release-candidate gate
  --with-gibbs-perf        pass through to release-candidate gate
  --gibbs-perf-baseline <csv>
                         pass through to release-candidate gate
  --gibbs-perf-current <csv>
                         pass through to release-candidate gate
  --with-gpuarray-f64-routes
                         pass through to release-candidate gate
  --gpuarray-f64-baseline <csv>
                         pass through to release-candidate gate
  --gpuarray-f64-repeats <n>
                         pass through to release-candidate gate
  --gpuarray-f64-profile <name>
                         pass through to release-candidate gate
  --verbose                verbose output
  --help, -h               show this help text

Exit codes:
  0 success
  1 failing check
  2 usage error
USAGE
}

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
REPORT_PATH="$REPO_ROOT/docs/BASELINES/RELEASE_REPORT_LATEST.md"

ALLOW_MISMATCH=0
ALLOW_ARTIFACTS=0
ALLOW_DRIFT=0
ALLOW_FALLBACK_GROWTH=0
WITH_TESTS=0
WITH_PERF=1
WITH_GIBBS_PERF=0
WITH_GPUARRAY_F64_ROUTES=0
PROFILE="legacy"
VERBOSE=0
PERF_BASELINE=""
PERF_CURRENT=""
GIBBS_PERF_BASELINE=""
GIBBS_PERF_CURRENT=""
GPUARRAY_F64_BASELINE=""
GPUARRAY_F64_REPEATS=""
GPUARRAY_F64_PROFILE=""

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --report) REPORT_PATH="${2:-}"; shift 2 ;;
    --profile) PROFILE="${2:-}"; shift 2 ;;
    --allow-mismatch) ALLOW_MISMATCH=1; shift ;;
    --allow-artifacts) ALLOW_ARTIFACTS=1; shift ;;
    --allow-drift) ALLOW_DRIFT=1; shift ;;
    --allow-fallback-growth) ALLOW_FALLBACK_GROWTH=1; shift ;;
    --with-tests) WITH_TESTS=1; shift ;;
    --with-perf) WITH_PERF=1; shift ;;
    --without-perf) WITH_PERF=0; shift ;;
    --with-gibbs-perf) WITH_GIBBS_PERF=1; shift ;;
    --with-gpuarray-f64-routes) WITH_GPUARRAY_F64_ROUTES=1; shift ;;
    --perf-baseline) PERF_BASELINE="${2:-}"; shift 2 ;;
    --perf-current) PERF_CURRENT="${2:-}"; shift 2 ;;
    --gibbs-perf-baseline) GIBBS_PERF_BASELINE="${2:-}"; shift 2 ;;
    --gibbs-perf-current) GIBBS_PERF_CURRENT="${2:-}"; shift 2 ;;
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

case "$PROFILE" in
  legacy|release-freeze) ;;
  *)
    echo "--profile must be one of: legacy, release-freeze." >&2
    exit 2
    ;;
esac

if [ "$WITH_PERF" -eq 1 ] && { [[ -z "$PERF_BASELINE" ]] || [[ -z "$PERF_CURRENT" ]]; }; then
  echo "perf guardrail is enabled by default and requires --perf-baseline and --perf-current." >&2
  exit 2
fi
if [ "$WITH_GIBBS_PERF" -eq 1 ] && { [[ -z "$GIBBS_PERF_BASELINE" ]] || [[ -z "$GIBBS_PERF_CURRENT" ]]; }; then
  echo "--with-gibbs-perf requires --gibbs-perf-baseline and --gibbs-perf-current." >&2
  exit 2
fi

cd "$REPO_ROOT"

tmp_ready="$(mktemp)"
tmp_rc="$(mktemp)"
trap 'rm -f "$tmp_ready" "$tmp_rc"' EXIT

readiness_status=0
rc_status=0
generated_at="$(date -u '+%Y-%m-%d %H:%M:%S UTC')"

if [ "$VERBOSE" -eq 1 ]; then
  echo "[run] release readiness"
fi
set +e
bash "$REPO_ROOT/scripts/check_release_readiness.sh" >"$tmp_ready" 2>&1
readiness_status=$?
set -e

if [ "$readiness_status" -eq 0 ]; then
  if [ "$VERBOSE" -eq 1 ]; then
    echo "[run] release candidate gate"
  fi

  set -- bash "$REPO_ROOT/scripts/check_release_candidate.sh"
  set -- "$@" --profile "$PROFILE"
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
  if [ "$WITH_GIBBS_PERF" -eq 1 ]; then
    set -- "$@" --with-gibbs-perf --gibbs-perf-baseline "$GIBBS_PERF_BASELINE" --gibbs-perf-current "$GIBBS_PERF_CURRENT"
  fi
  if [ "$WITH_GPUARRAY_F64_ROUTES" -eq 1 ]; then
    set -- "$@" --with-gpuarray-f64-routes
  fi
  if [[ -n "$GPUARRAY_F64_BASELINE" ]]; then
    set -- "$@" --gpuarray-f64-baseline "$GPUARRAY_F64_BASELINE"
  fi
  if [[ -n "$GPUARRAY_F64_REPEATS" ]]; then
    set -- "$@" --gpuarray-f64-repeats "$GPUARRAY_F64_REPEATS"
  fi
  if [[ -n "$GPUARRAY_F64_PROFILE" ]]; then
    set -- "$@" --gpuarray-f64-profile "$GPUARRAY_F64_PROFILE"
  fi
  if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi

  set +e
  "$@" >"$tmp_rc" 2>&1
  rc_status=$?
  set -e
else
  echo "Skipped release-candidate gate due readiness failure." >"$tmp_rc"
fi

mkdir -p "$(dirname "$REPORT_PATH")"
{
  echo "# Release Validation Report"
  echo
  echo "- Generated: $generated_at"
  echo "- Readiness status: $readiness_status"
  echo "- Release-candidate status: $rc_status"
  echo "- Overall: $([ "$readiness_status" -eq 0 ] && [ "$rc_status" -eq 0 ] && echo "PASS" || echo "FAIL")"
  echo
  echo "## Readiness Output"
  echo
  echo '```text'
  cat "$tmp_ready"
  echo '```'
  echo
  echo "## Release Candidate Output"
  echo
  echo '```text'
  cat "$tmp_rc"
  echo '```'
} > "$REPORT_PATH"

if [ "$VERBOSE" -eq 1 ]; then
  echo "Report written to: $REPORT_PATH"
fi

if [ "$readiness_status" -ne 0 ] || [ "$rc_status" -ne 0 ]; then
  exit 1
fi

echo "Release validation run: clean"
exit 0
