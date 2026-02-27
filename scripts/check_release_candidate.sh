#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_release_candidate.sh [options]

Release-candidate closeout gate. Runs:
  - legacy profile:
    1) continuous verification gates
    2) behavior fallback isolation growth check
    3) optional perf guardrail comparison
    4) optional Gibbs perf guardrail comparison
    5) optional GPUArray f64 route gate
  - release-freeze profile:
    1) repository hygiene (allow known backend binary artifacts)
    2) package tarball gate
    3) backend bundle symbols
    4) behavior fallback isolation growth check
    5) perf guardrail comparison (required)
    6) GPUArray math route+reason and wrapper/context stability lanes (GPU-only)

Options:
  --profile <legacy|release-freeze>
                          gate profile (default: legacy)
  --allow-mismatch        pass through to continuous verification
  --allow-artifacts       pass through to continuous verification
  --allow-drift           pass through to continuous verification
  --allow-fallback-growth allow fallback hotspot growth
  --with-tests            pass through to continuous verification
  --with-perf             run perf guardrail (default)
  --without-perf          disable perf guardrail
  --perf-baseline <csv>   perf baseline csv path
  --perf-current <csv>    perf current csv path
  --with-gibbs-perf       run Gibbs perf guardrail
  --gibbs-perf-baseline <csv>
                          Gibbs perf baseline csv path
  --gibbs-perf-current <csv>
                          Gibbs perf current csv path
  --with-gpuarray-f64-routes
                          run GPUArray f64 route audit gate (default)
  --without-gpuarray-f64-routes
                          skip GPUArray f64 route audit gate
  --gpuarray-f64-baseline <csv>
                          baseline csv for GPUArray f64 route gate
  --gpuarray-f64-repeats <n>
                          repeats for GPUArray f64 route audit (default: 8)
  --gpuarray-f64-profile <name>
                          gate profile for f64 route audit (default: closeout)
  --verbose               verbose output
  --help, -h              show help

Exit codes:
  0 clean
  1 failing gate(s)
  2 usage error
USAGE
}

ALLOW_MISMATCH=0
ALLOW_ARTIFACTS=0
ALLOW_DRIFT=0
ALLOW_FALLBACK_GROWTH=0
WITH_TESTS=0
WITH_PERF=1
WITH_GIBBS_PERF=0
WITH_GPUARRAY_F64_ROUTES=1
PROFILE="legacy"
VERBOSE=0
PERF_BASELINE=""
PERF_CURRENT=""
GIBBS_PERF_BASELINE=""
GIBBS_PERF_CURRENT=""
GPUARRAY_F64_BASELINE=""
GPUARRAY_F64_REPEATS="8"
GPUARRAY_F64_PROFILE="closeout"

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
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
    --without-gpuarray-f64-routes) WITH_GPUARRAY_F64_ROUTES=0; shift ;;
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

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

case "$PROFILE" in
  legacy|release-freeze) ;;
  *)
    echo "--profile must be one of: legacy, release-freeze." >&2
    exit 2
    ;;
esac

if [[ -z "$GPUARRAY_F64_BASELINE" ]]; then
  GPUARRAY_F64_BASELINE="$REPO_ROOT/docs/BASELINES/GPUARRAY_F64_ROUTE_BASELINE.csv"
fi

if [[ ! "$GPUARRAY_F64_REPEATS" =~ ^[0-9]+$ || "$GPUARRAY_F64_REPEATS" -lt 1 ]]; then
  echo "--gpuarray-f64-repeats must be a positive integer." >&2
  exit 2
fi

case "$GPUARRAY_F64_PROFILE" in
  closeout|targeted-drop) ;;
  *)
    echo "--gpuarray-f64-profile must be one of: closeout, targeted-drop." >&2
    exit 2
    ;;
esac

run_step() {
  local label="$1"
  shift
  if [ "$VERBOSE" -eq 1 ]; then
    echo "[run] $label: $*"
  else
    echo "[run] $label"
  fi
  if "$@"; then
    echo "[ok ] $label"
    return 0
  fi
  echo "[err] $label"
  return 1
}

detect_package_gpu() {
  Rscript -e '
    source("packages/mojor/R/mojor.R")
    ok <- FALSE
    if (file.exists("packages/mojor/build/mojor_bridge.so")) {
      try(mojor_load("packages/mojor/build"), silent = TRUE)
      ok <- isTRUE(mojor_is_loaded()) && isTRUE(mojor_has_gpu())
    }
    if (isTRUE(ok)) {
      cat("1\n")
    } else {
      cat("0\n")
    }
  '
}

failures=0

if [ "$PROFILE" = "release-freeze" ]; then
  if [ "$ALLOW_MISMATCH" -eq 1 ] || [ "$ALLOW_ARTIFACTS" -eq 1 ] || [ "$ALLOW_DRIFT" -eq 1 ] || [ "$ALLOW_FALLBACK_GROWTH" -eq 1 ]; then
    echo "release-freeze profile does not allow --allow-mismatch/--allow-artifacts/--allow-drift/--allow-fallback-growth." >&2
    exit 2
  fi
  if [ "$WITH_TESTS" -eq 1 ]; then
    echo "release-freeze profile does not accept --with-tests (legacy lane)." >&2
    exit 2
  fi
  if [ "$WITH_GIBBS_PERF" -eq 1 ]; then
    echo "release-freeze profile does not accept --with-gibbs-perf." >&2
    exit 2
  fi
  if [ "$WITH_GPUARRAY_F64_ROUTES" -eq 1 ]; then
    [ "$VERBOSE" -eq 1 ] && echo "release-freeze profile: forcing legacy --with-gpuarray-f64-routes lane off."
    WITH_GPUARRAY_F64_ROUTES=0
  fi
  if [ "$WITH_PERF" -ne 1 ]; then
    echo "release-freeze profile requires perf guardrail; do not use --without-perf." >&2
    exit 2
  fi
  if [[ -z "$PERF_BASELINE" || -z "$PERF_CURRENT" ]]; then
    echo "release-freeze profile requires --perf-baseline and --perf-current." >&2
    exit 2
  fi

  set -- bash "$REPO_ROOT/scripts/check_repo_hygiene.sh" --allow-artifacts
  if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
  run_step "repository hygiene" "$@" || failures=$((failures + 1))

  set -- bash "$REPO_ROOT/scripts/check_package_tarball.sh"
  if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
  run_step "package tarball" "$@" || failures=$((failures + 1))

  set -- bash "$REPO_ROOT/scripts/check_backend_bundle_symbols.sh" --package-dir "$REPO_ROOT/packages/mojor"
  if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
  run_step "backend bundle symbols" "$@" || failures=$((failures + 1))

  set -- bash "$REPO_ROOT/scripts/check_fallback_isolation.sh"
  if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
  run_step "fallback isolation" "$@" || failures=$((failures + 1))

  set -- bash "$REPO_ROOT/scripts/check_perf_guardrail.sh" --baseline "$PERF_BASELINE" --current "$PERF_CURRENT"
  if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
  run_step "perf guardrail" "$@" || failures=$((failures + 1))

  gpu_available="$(detect_package_gpu | tr -d '[:space:]')"
  if [ "$gpu_available" = "1" ]; then
    run_step "gpuarray math route gate" \
      Rscript "$REPO_ROOT/packages/mojor/tools/audit_gpuarray_math_routes.R" \
      --run-profile=ci-fast \
      --baseline="$REPO_ROOT/docs/BASELINES/GPUARRAY_MATH_ROUTE_BASELINE.csv" \
      --reason-baseline="$REPO_ROOT/docs/BASELINES/GPUARRAY_MATH_REASON_BASELINE.csv" || failures=$((failures + 1))

    run_step "gpuarray math wrapper regression" \
      Rscript "$REPO_ROOT/packages/mojor/tools/check_gpuarray_math_wrapper_regression.R" \
      --iters=12 \
      --quiet-fallback-warnings=true || failures=$((failures + 1))

    run_step "gpuarray context stability" \
      Rscript "$REPO_ROOT/scripts/check_gpuarray_context_stability.R" \
      --file="$REPO_ROOT/packages/mojor/tests/testthat/test_gpu_array.R" \
      --repeats=2 \
      --reporter=summary \
      --reset_mode=soft \
      --file_reset_mode=soft \
      --per_test_reset=false \
      --disable_r_jit=true \
      --stress_opt_level=0 || failures=$((failures + 1))
  else
    echo "[skip] gpuarray package-only lanes (GPU unavailable)"
  fi
else
  set -- bash "$REPO_ROOT/scripts/check_continuous_verification.sh"
  if [ "$ALLOW_MISMATCH" -eq 1 ]; then set -- "$@" --allow-mismatch; fi
  if [ "$ALLOW_ARTIFACTS" -eq 1 ]; then set -- "$@" --allow-artifacts; fi
  if [ "$ALLOW_DRIFT" -eq 1 ]; then set -- "$@" --allow-drift; fi
  if [ "$WITH_TESTS" -eq 1 ]; then set -- "$@" --with-tests; fi
  if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
  run_step "continuous verification" "$@" || failures=$((failures + 1))

  set -- bash "$REPO_ROOT/scripts/check_fallback_isolation.sh"
  if [ "$ALLOW_FALLBACK_GROWTH" -eq 1 ]; then set -- "$@" --allow-growth; fi
  if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
  run_step "fallback isolation" "$@" || failures=$((failures + 1))
fi

if [ "$PROFILE" = "legacy" ]; then
  if [ "$WITH_PERF" -eq 1 ]; then
    if [[ -z "$PERF_BASELINE" || -z "$PERF_CURRENT" ]]; then
      echo "perf guardrail is enabled by default and requires --perf-baseline and --perf-current." >&2
      exit 2
    fi
    set -- bash "$REPO_ROOT/scripts/check_perf_guardrail.sh" --baseline "$PERF_BASELINE" --current "$PERF_CURRENT"
    if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
    run_step "perf guardrail" "$@" || failures=$((failures + 1))
  fi

  if [ "$WITH_GIBBS_PERF" -eq 1 ]; then
    if [[ -z "$GIBBS_PERF_BASELINE" || -z "$GIBBS_PERF_CURRENT" ]]; then
      echo "--with-gibbs-perf requires --gibbs-perf-baseline and --gibbs-perf-current." >&2
      exit 2
    fi
    set -- bash "$REPO_ROOT/scripts/check_gibbs_perf_guardrail.sh" --baseline "$GIBBS_PERF_BASELINE" --current "$GIBBS_PERF_CURRENT"
    if [ "$VERBOSE" -eq 1 ]; then set -- "$@" --verbose; fi
    run_step "gibbs perf guardrail" "$@" || failures=$((failures + 1))
  fi

  if [ "$WITH_GPUARRAY_F64_ROUTES" -eq 1 ]; then
    if [[ ! -f "$GPUARRAY_F64_BASELINE" ]]; then
      echo "GPUArray f64 baseline not found: $GPUARRAY_F64_BASELINE" >&2
      exit 2
    fi

    set -- Rscript "$REPO_ROOT/packages/mojor/tools/audit_gpuarray_f64_routes.R" \
      --repeats="$GPUARRAY_F64_REPEATS" \
      --baseline="$GPUARRAY_F64_BASELINE" \
      --profile="$GPUARRAY_F64_PROFILE"
    run_step "gpuarray f64 routes" "$@" || failures=$((failures + 1))
  fi
fi

if [ "$failures" -ne 0 ]; then
  echo "Release-candidate gate failed with $failures failing step(s)."
  exit 1
fi

echo "Release-candidate gate: clean"
exit 0
