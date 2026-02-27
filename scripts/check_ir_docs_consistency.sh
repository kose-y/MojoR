#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_ir_docs_consistency.sh [--help]

Checks targeted IR documentation consistency:
  1) active IR docs do not contain legacy path tokens
  2) contradiction-prone claims use canonical wording

Exit codes:
  0 clean
  1 consistency violation
  2 usage/input error
USAGE
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi
if [[ "${1:-}" != "" ]]; then
  echo "Unknown argument: $1" >&2
  usage
  exit 2
fi

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

violation=0

require_text() {
  local file="$1"
  local pattern="$2"
  local label="$3"
  if ! grep -Fq "$pattern" "$file"; then
    echo "IR docs consistency: missing $label in $file"
    violation=1
  fi
}

legacy_token="$(printf 'prototype%s' '/')"
if command -v rg >/dev/null 2>&1; then
  if rg -n "$legacy_token" docs/IR >/dev/null; then
    echo "IR docs consistency: legacy path token found in active IR docs"
    rg -n "$legacy_token" docs/IR || true
    violation=1
  fi
else
  if grep -R -n "$legacy_token" docs/IR >/dev/null 2>&1; then
    echo "IR docs consistency: legacy path token found in active IR docs"
    grep -R -n "$legacy_token" docs/IR || true
    violation=1
  fi
fi

require_text "docs/IR/PLAN.md" "matrix/ND strict/layout completeness remains partial" "typed-IR partial-coverage wording"
require_text "docs/IR/FUNCTION_SUPPORT_BREADTH.md" "runif" "RNG coverage table entry"
require_text "docs/IR/FUNCTION_SUPPORT_BREADTH.md" "rng_vec" "rng_vec lane wording"
rng_limit_phrase='Vectorized RNG lanes are implemented for `runif`/`rnorm`'
require_text "docs/KNOWN_ISSUES.md" "$rng_limit_phrase" "RNG limitation wording"
require_text "docs/KNOWN_ISSUES.md" "Matrix/ND typed layout completeness remains partial" "typed-IR limitation wording"

if [[ "$violation" -ne 0 ]]; then
  exit 1
fi

echo "IR docs consistency check: clean"
exit 0
