#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_release_readiness.sh [--verbose] [--allow-missing-workflow] [--help]

Checks local release prerequisites:
  1) required gate scripts exist and are executable
  2) required milestone baseline files exist
  3) CI workflow file exists (optional bypass)

Exit codes:
  0 ready
  1 not ready
  2 usage error
USAGE
}

VERBOSE=0
ALLOW_MISSING_WORKFLOW=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --verbose) VERBOSE=1; shift ;;
    --allow-missing-workflow) ALLOW_MISSING_WORKFLOW=1; shift ;;
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

REQUIRED_EXEC_SCRIPTS=(
  "$REPO_ROOT/scripts/check_mirror_parity.sh"
  "$REPO_ROOT/scripts/check_repo_hygiene.sh"
  "$REPO_ROOT/scripts/check_release_surface.sh"
  "$REPO_ROOT/scripts/check_continuous_verification.sh"
  "$REPO_ROOT/scripts/check_perf_guardrail.sh"
  "$REPO_ROOT/scripts/check_fallback_isolation.sh"
  "$REPO_ROOT/scripts/check_backend_bundle_symbols.sh"
  "$REPO_ROOT/scripts/check_release_candidate.sh"
  "$REPO_ROOT/scripts/check_gpuarray_context_stability.R"
)

REQUIRED_FILES=(
  "$REPO_ROOT/docs/BASELINES/RELEASE_SURFACE_BASELINE.md"
  "$REPO_ROOT/docs/BASELINES/PERF_BASELINE_SAMPLE.csv"
  "$REPO_ROOT/docs/BASELINES/FALLBACK_HOTSPOTS_BASELINE.csv"
  "$REPO_ROOT/docs/BASELINES/GPUARRAY_MATH_ROUTE_BASELINE.csv"
  "$REPO_ROOT/docs/BASELINES/GPUARRAY_MATH_REASON_BASELINE.csv"
  "$REPO_ROOT/scripts/fallback_behavior_patterns.txt"
  "$REPO_ROOT/docs/BASELINES/PHASE5_CONTINUOUS_VERIFICATION.md"
  "$REPO_ROOT/docs/BASELINES/PHASE6_RELEASE_CANDIDATE_CLOSEOUT.md"
  "$REPO_ROOT/docs/BASELINES/RELEASE_FREEZE_CHECKLIST.md"
)

WORKFLOW_FILE="$REPO_ROOT/.github/workflows/ci.yml"
violation=0

for f in "${REQUIRED_EXEC_SCRIPTS[@]}"; do
  if [ ! -f "$f" ]; then
    echo "Release readiness: missing script: $f"
    violation=1
    continue
  fi
  if [ ! -x "$f" ]; then
    echo "Release readiness: script is not executable: $f"
    violation=1
  fi
done

for f in "${REQUIRED_FILES[@]}"; do
  if [ ! -f "$f" ]; then
    echo "Release readiness: missing file: $f"
    violation=1
  fi
done

if [ ! -f "$WORKFLOW_FILE" ]; then
  if [ "$ALLOW_MISSING_WORKFLOW" -eq 1 ]; then
    [ "$VERBOSE" -eq 1 ] && echo "Release readiness: missing workflow allowed (--allow-missing-workflow)."
  else
    echo "Release readiness: missing workflow file: $WORKFLOW_FILE"
    violation=1
  fi
fi

if [ "$VERBOSE" -eq 1 ]; then
  echo "Release readiness checks complete."
fi

if [ "$violation" -ne 0 ]; then
  exit 1
fi

echo "Release readiness check: ready"
exit 0
