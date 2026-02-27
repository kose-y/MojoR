#!/usr/bin/env bash
set -euo pipefail

# Run MojoR testthat files in isolated R subprocesses.
# This avoids long-lived single-process instability seen on some Linux hosts.

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="${MOJOR_TEST_DIR:-$ROOT/tests/testthat}"
R_BIN="${R_BIN:-Rscript}"
REPORTER="${MOJOR_TEST_REPORTER:-summary}"
LOG_DIR="${MOJOR_TEST_LOG_DIR:-/tmp/mojor_testthat_isolated_logs}"

# Ensure toolchain commands (e.g., mojo) from the chosen R environment are visible.
R_BIN_DIR="$(cd "$(dirname "$R_BIN")" && pwd)"
export PATH="$R_BIN_DIR:$PATH"

if [ ! -d "$TEST_DIR" ]; then
  echo "test directory not found: $TEST_DIR" >&2
  exit 2
fi

mkdir -p "$LOG_DIR"

if [ "$#" -gt 0 ]; then
  FILES=("$@")
else
  mapfile -t FILES < <(find "$TEST_DIR" -maxdepth 1 -type f -name 'test_*.R' | sort)
fi

if [ "${#FILES[@]}" -eq 0 ]; then
  echo "no test files found"
  exit 0
fi

TOTAL=0
FAILED=0
CRASHED=0

for f in "${FILES[@]}"; do
  if [ ! -f "$f" ]; then
    echo "skip missing file: $f" >&2
    continue
  fi

  TOTAL=$((TOTAL + 1))
  base="$(basename "$f" .R)"
  log="$LOG_DIR/${base}.log"

  echo "[${TOTAL}] $f"
  set +e
  "$R_BIN" -e "testthat::test_file('$f', reporter='$REPORTER', stop_on_failure=TRUE, stop_on_warning=FALSE)" >"$log" 2>&1
  rc=$?
  set -e

  if [ "$rc" -eq 0 ]; then
    echo "  PASS"
    continue
  fi

  FAILED=$((FAILED + 1))
  if [ "$rc" -eq 139 ]; then
    CRASHED=$((CRASHED + 1))
    echo "  CRASH (rc=139) log=$log"
  else
    echo "  FAIL (rc=$rc) log=$log"
  fi
done

echo
echo "total=$TOTAL failed=$FAILED crashed=$CRASHED logs=$LOG_DIR"

if [ "$FAILED" -gt 0 ]; then
  exit 1
fi

exit 0
