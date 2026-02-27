#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

usage() {
  cat <<'USAGE'
Usage: repro_gibbs_runs.sh [N] [THIN] [REPS] [N_JIT] [THIN_JIT] [OUT_DIR] [--write-current-csv <path>]

Runs reproducible Gibbs benchmark commands and optional direct JIT introspection.

Positional defaults:
  N=2000 THIN=50 REPS=3 N_JIT=300 THIN_JIT=20
  OUT_DIR=artifacts/gibbs_repro_<timestamp>

Optional flags:
  --write-current-csv <path>  parse baseline log and write name,ms CSV using parse_gibbs_benchmark_log.R
USAGE
}

WRITE_CURRENT_CSV=""
POSITIONAL=()
while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --write-current-csv)
      WRITE_CURRENT_CSV="${2:-}"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      POSITIONAL+=("${1:-}")
      shift
      ;;
  esac
done

if ((${#POSITIONAL[@]} > 0)); then
  set -- "${POSITIONAL[@]}"
else
  set --
fi

N="${1:-2000}"
THIN="${2:-50}"
REPS="${3:-3}"
N_JIT="${4:-300}"
THIN_JIT="${5:-20}"
OUT_DIR="${6:-$REPO_ROOT/artifacts/gibbs_repro_$(date +%Y%m%d_%H%M%S)}"

mkdir -p "$OUT_DIR"

echo "Repo root: $REPO_ROOT"
echo "Output dir: $OUT_DIR"
echo "Benchmark args: N=$N THIN=$THIN REPS=$REPS"
echo "JIT-info args: N_JIT=$N_JIT THIN_JIT=$THIN_JIT"

run_and_log() {
  local name="$1"
  shift
  echo
  echo "== $name =="
  echo "Command: $*"
  "$@" | tee "$OUT_DIR/${name}.log"
}

run_and_log "gibbs_baseline_no_bulk_nojit" \
  Rscript "$REPO_ROOT/gibbs_benchmark.R" "$N" "$THIN" "$REPS" 0 0

run_and_log "gibbs_with_bulk_nojit" \
  Rscript "$REPO_ROOT/gibbs_benchmark.R" "$N" "$THIN" "$REPS" 1 0

run_and_log "gibbs_baseline_with_jit3" \
  Rscript "$REPO_ROOT/gibbs_benchmark.R" "$N" "$THIN" "$REPS" 0 1

run_and_log "gibbs_direct_jit_info" \
  Rscript "$REPO_ROOT/scripts/repro_gibbs_jit_info.R" "$N_JIT" "$THIN_JIT"

if [[ -n "$WRITE_CURRENT_CSV" ]]; then
  Rscript "$REPO_ROOT/scripts/parse_gibbs_benchmark_log.R" \
    --input="$OUT_DIR/gibbs_baseline_no_bulk_nojit.log" \
    --output="$WRITE_CURRENT_CSV"
fi

echo
echo "Done. Logs: $OUT_DIR"
