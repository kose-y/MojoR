#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: run_testthat_parallel_sweep.sh [options]

Runs package testthat files in file-level parallel workers and records per-file PASS/FAIL.

Options:
  --jobs N           Parallel workers (default: 25)
  --target NAME      package (default: package; legacy values packages/mojor/both are rejected)
  --exclude-file BASENAME
                    Exclude one test file basename (e.g. test_indexing_comprehensive.R)
  --out-root DIR     Output directory root (default: <repo>/.tmp)
  --help, -h         Show this help text

Output:
  Writes:
   <run_root>/files.txt
   <run_root>/logs/*.log
   <run_root>/status/*.tsv
   <run_root>/results.tsv

Exit code:
  0 when all selected package tests pass
  1 when one or more files fail
  2 for usage/configuration errors
USAGE
}

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
JOBS=25
TARGET="package"
OUT_ROOT="$REPO_ROOT/.tmp"
EXCLUDE_FILE=""
R_BIN="${R_BIN:-Rscript}"
R_BIN_DIR="$(cd "$(dirname "$R_BIN")" && pwd)"
export PATH="$R_BIN_DIR:$PATH"

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --jobs)
      JOBS="${2:-}"
      shift 2
      ;;
    --target)
      TARGET="${2:-}"
      shift 2
      ;;
    --exclude-file)
      EXCLUDE_FILE="${2:-}"
      shift 2
      ;;
    --out-root)
      OUT_ROOT="${2:-}"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if ! [[ "$JOBS" =~ ^[0-9]+$ ]] || [[ "$JOBS" -lt 1 ]]; then
  echo "--jobs must be a positive integer" >&2
  exit 2
fi

case "$TARGET" in
  package) ;;
  prototype|both)
    echo "--target '$TARGET' is no longer supported: packages/mojor/ has been removed; use --target package." >&2
    exit 2
    ;;
  *)
    echo "--target must be: package" >&2
    exit 2
    ;;
esac

mkdir -p "$OUT_ROOT"
cd "$REPO_ROOT"

run_suite() {
  local suite="package"
  local test_dir="$REPO_ROOT/packages/mojor/tests/testthat"
  local lib_path="$REPO_ROOT/packages/mojor/build"
  local run_root="$OUT_ROOT/mojor_p${JOBS}_${suite}_$(date +%s)"

  mkdir -p "$run_root/logs" "$run_root/status"

  find "$test_dir" -maxdepth 1 -type f -name 'test_*.R' | sort > "$run_root/files.txt"
  if [[ -n "$EXCLUDE_FILE" ]]; then
    grep -v "/${EXCLUDE_FILE}$" "$run_root/files.txt" > "$run_root/files.filtered.txt" || true
    mv "$run_root/files.filtered.txt" "$run_root/files.txt"
  fi
  if [[ ! -s "$run_root/files.txt" ]]; then
    echo "No test files found in $test_dir" >&2
    return 2
  fi

  xargs -P"$JOBS" -I{} bash -lc '
    f="$1"
    run_root="$2"
    lib_path="$3"
    r_bin="$4"
    base=$(basename "$f" .R)
    log="$run_root/logs/${base}.log"
    status="$run_root/status/${base}.tsv"
    cache_dir="$run_root/cache/${base}"
    mkdir -p "$cache_dir"

    MOJOR_LIB_PATH="$lib_path" \
    MOJOR_CACHE_DIR="$cache_dir" \
    "$r_bin" -e "testthat::test_file(\"$f\", reporter = \"summary\", stop_on_failure = TRUE, stop_on_warning = FALSE)" >"$log" 2>&1
    ec=$?

    if [ "$ec" -ne 0 ]; then
      st=FAIL
    else
      st=PASS
    fi

    printf "%s\t%s\t%s\t%s\n" "$st" "$ec" "$f" "$log" > "$status"
  ' _ {} "$run_root" "$lib_path" "$R_BIN" < "$run_root/files.txt"

  cat "$run_root/status"/*.tsv | sort > "$run_root/results.tsv"
  local total
  local fails
  total=$(wc -l < "$run_root/results.tsv")
  fails=$(awk -F'\t' '$1=="FAIL"{c++} END{print c+0}' "$run_root/results.tsv")

  echo "RUN_ROOT=$run_root"
  echo "SUITE=$suite"
  echo "TOTAL=$total"
  echo "FAILS=$fails"
  awk -F'\t' '$1=="FAIL"{print $0}' "$run_root/results.tsv" | sed -n '1,200p'

  [[ "$fails" -eq 0 ]]
}

if run_suite; then
  exit 0
fi
exit 1
