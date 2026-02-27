#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_fallback_isolation.sh [--baseline <file.csv>] [--patterns <file.txt>] [--allow-growth] [--verbose]

Checks behavior-anchored fallback hotspot growth across the authoritative R tree:
  - packages/mojor/R

Baseline CSV format:
  scope,max_hits
  packages/mojor/R,5

Exit codes:
  0 clean (no growth)
  1 growth detected
  2 usage/input error
USAGE
}

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEFAULT_BASELINE="$REPO_ROOT/docs/BASELINES/FALLBACK_HOTSPOTS_BASELINE.csv"
DEFAULT_PATTERNS="$REPO_ROOT/scripts/fallback_behavior_patterns.txt"
BASELINE="$DEFAULT_BASELINE"
PATTERNS_FILE="$DEFAULT_PATTERNS"
ALLOW_GROWTH=0
VERBOSE=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --baseline)
      BASELINE="${2:-}"
      shift 2
      ;;
    --patterns)
      PATTERNS_FILE="${2:-}"
      shift 2
      ;;
    --allow-growth)
      ALLOW_GROWTH=1
      shift
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

if [ ! -f "$BASELINE" ]; then
  echo "Fallback isolation check: baseline file not found: $BASELINE" >&2
  exit 2
fi
if [ ! -f "$PATTERNS_FILE" ]; then
  echo "Fallback isolation check: patterns file not found: $PATTERNS_FILE" >&2
  exit 2
fi

cd "$REPO_ROOT"

SCOPES=(
  "packages/mojor/R"
)

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

# Normalize pattern file: ignore blank/comment lines.
awk '
  {
    line = $0
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", line)
    if (line == "" || substr(line, 1, 1) == "#") next
    print line
  }
' "$PATTERNS_FILE" > "$tmpdir/patterns.txt"
if [ ! -s "$tmpdir/patterns.txt" ]; then
  echo "Fallback isolation check: patterns file has no active patterns: $PATTERNS_FILE" >&2
  exit 2
fi

awk -F, '
  NR == 1 { next }
  NF >= 2 {
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", $1)
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", $2)
    if ($1 != "" && $2 != "") print $1 "," $2
  }
' "$BASELINE" > "$tmpdir/baseline.csv"

violation=0

count_hits() {
  local scope="$1"
  local out
  out="$(rg -n -S -f "$tmpdir/patterns.txt" "$REPO_ROOT/$scope" || true)"
  if [[ -z "$out" ]]; then
    echo 0
  else
    printf "%s\n" "$out" | wc -l | tr -d '[:space:]'
  fi
}

for scope in "${SCOPES[@]}"; do
  baseline_hits="$(awk -F, -v s="$scope" '$1 == s { print $2 }' "$tmpdir/baseline.csv" | head -n 1)"
  if [[ -z "$baseline_hits" ]]; then
    echo "Fallback isolation check: scope '$scope' missing from baseline." >&2
    exit 2
  fi

  current_hits="$(count_hits "$scope")"
  delta=$(( current_hits - baseline_hits ))

  echo "$scope: current=$current_hits baseline=$baseline_hits delta=$delta"

  if [ "$current_hits" -gt "$baseline_hits" ]; then
    violation=1
  fi
done

if [ "$VERBOSE" -eq 1 ]; then
  echo "Top fallback behavior hotspots:"
  all_hits="$(rg -n -S -f "$tmpdir/patterns.txt" "$REPO_ROOT/packages/mojor/R" || true)"
  if [[ -z "$all_hits" ]]; then
    echo "<none>"
  else
    printf "%s\n" "$all_hits" \
      | awk -F: '{print $1}' \
      | sed "s#^$REPO_ROOT/##" \
      | sort | uniq -c | sort -nr | head -n 20 \
      | sed 's/^ *//'
  fi
fi

if [ "$violation" -ne 0 ]; then
  if [ "$ALLOW_GROWTH" -eq 1 ]; then
    echo "Fallback hotspot growth detected, continuing (--allow-growth)."
    exit 0
  fi
  echo "Fallback hotspot growth detected."
  exit 1
fi

echo "Fallback isolation check: clean"
exit 0
