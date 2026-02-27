#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_continuous_verification.sh [options]

Runs continuous verification gates in sequence:
  1) migration token guard
  2) repository hygiene
  3) release surface stability
  4) IR docs consistency
  5) package tarball build/check
  6) optional representative high-signal tests

Options:
  --allow-mismatch   Pass through to check_mirror_parity.sh.
  --allow-artifacts  Pass through to check_repo_hygiene.sh.
  --allow-drift      Pass through to check_release_surface.sh.
  --skip-package-tarball  Skip package tarball gate.
  --with-tests       Run high-signal tests after gate checks.
  --verbose          Print executed commands.
  --help, -h         Show this help text.

Exit codes:
  0 all checks passed
  1 one or more checks failed
  2 usage error
USAGE
}

ALLOW_MISMATCH=0
ALLOW_ARTIFACTS=0
ALLOW_DRIFT=0
SKIP_PACKAGE_TARBALL=0
WITH_TESTS=0
VERBOSE=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --allow-mismatch) ALLOW_MISMATCH=1; shift ;;
    --allow-artifacts) ALLOW_ARTIFACTS=1; shift ;;
    --allow-drift) ALLOW_DRIFT=1; shift ;;
    --skip-package-tarball) SKIP_PACKAGE_TARBALL=1; shift ;;
    --with-tests) WITH_TESTS=1; shift ;;
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

run_gate() {
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

failures=0

if [ "$ALLOW_MISMATCH" -eq 1 ]; then
  run_gate "migration token guard" bash "$REPO_ROOT/scripts/check_mirror_parity.sh" --allow-mismatch || failures=$((failures + 1))
else
  run_gate "migration token guard" bash "$REPO_ROOT/scripts/check_mirror_parity.sh" || failures=$((failures + 1))
fi

if [ "$ALLOW_ARTIFACTS" -eq 1 ]; then
  run_gate "repository hygiene" bash "$REPO_ROOT/scripts/check_repo_hygiene.sh" --allow-artifacts || failures=$((failures + 1))
else
  run_gate "repository hygiene" bash "$REPO_ROOT/scripts/check_repo_hygiene.sh" || failures=$((failures + 1))
fi

if [ "$ALLOW_DRIFT" -eq 1 ]; then
  run_gate "release surface" bash "$REPO_ROOT/scripts/check_release_surface.sh" --allow-drift || failures=$((failures + 1))
else
  run_gate "release surface" bash "$REPO_ROOT/scripts/check_release_surface.sh" || failures=$((failures + 1))
fi

run_gate "IR docs consistency" bash "$REPO_ROOT/scripts/check_ir_docs_consistency.sh" || failures=$((failures + 1))

if [ "$SKIP_PACKAGE_TARBALL" -eq 1 ]; then
  [ "$VERBOSE" -eq 1 ] && echo "Skipping package tarball gate (--skip-package-tarball)."
else
  if [ "$VERBOSE" -eq 1 ]; then
    run_gate "package tarball" bash "$REPO_ROOT/scripts/check_package_tarball.sh" --verbose || failures=$((failures + 1))
  else
    run_gate "package tarball" bash "$REPO_ROOT/scripts/check_package_tarball.sh" || failures=$((failures + 1))
  fi
fi

if [ "$WITH_TESTS" -eq 1 ]; then
  if [ "$(uname -s)" = "Linux" ] && [ -x "$REPO_ROOT/packages/mojor/tools/run_testthat_isolated.sh" ]; then
    run_gate "linux isolated high-signal tests" \
      env R_BIN=Rscript "$REPO_ROOT/packages/mojor/tools/run_testthat_isolated.sh" \
      "$REPO_ROOT/packages/mojor/tests/testthat/test_ir_verify.R" \
      "$REPO_ROOT/packages/mojor/tests/testthat/test_transpile_ir_integration.R" || failures=$((failures + 1))
  else
    run_gate "test package ir verify" \
      Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_ir_verify.R")' || failures=$((failures + 1))
    run_gate "test package transpile ir integration" \
      Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_transpile_ir_integration.R")' || failures=$((failures + 1))
  fi
fi

if [ "$failures" -ne 0 ]; then
  echo "Continuous verification failed with $failures failing step(s)."
  exit 1
fi

echo "Continuous verification: clean"
exit 0
